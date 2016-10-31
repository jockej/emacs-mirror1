/*
n * Copyright (C) 2016  Joakim Jalap
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* This file implements an Arne Andersson-tree (AA-tree) for buffer
   overlays.  It is an augmented interval tree.  Basically each node
   has an interval and a 'max' value, which is the largest upper
   interval bound which appears in the subtree rooted in the node.

   The implementation of the tree functions is written as closely as
   possible to the one presented in Anderssons paper (REF)[1], but with a
   few differences.  The code in [1] is written in Pascal, which has
   proper pass by reference. Unfortunately C doesn't really have this;
   this is the reason for passing around pointers to pointers.

   Also this tree has parent references, and the code in the delete
   routine is a bit more complicated because we need to delete the
   actual memory area, so as to not trip up the GC.

   The fact that this is an augmented tree also makes the rebalancing
   operation (split and skew) a bit more complex.  */

#include "overlays.h"
#include "buffer.h"

/* Defined in buffer.c */
extern void
modify_overlay (struct buffer *, ptrdiff_t, ptrdiff_t);

/* Return the max of a, b and c.  */
static inline ptrdiff_t
overlay_max (ptrdiff_t a, ptrdiff_t b, ptrdiff_t c)
{
  ptrdiff_t bc_max = max(b, c);
  return max(a, bc_max);
}

/* Find what the max value should be for X.  */
static inline ptrdiff_t
overlay_find_max (struct Lisp_Overlay *x)
{
  return overlay_max(x->left->max, x->right->max, x->char_end);
}

/* The sentinel node.  This indicates the bottom of the tree.  It is
   basically a way to avoid having to check for NULL pointers all the
   time.  */
struct Lisp_Overlay OVERLAY_SENTINEL_NODE =
  {
    .type = Lisp_Misc_Overlay,          /* type */
    .gcmarkbit = 1,                     /* gcmarkbit */
    .left = &OVERLAY_SENTINEL_NODE,     /* left */
    .right = &OVERLAY_SENTINEL_NODE,    /* right */
    .level = 0,                          /* level */
    .parent = 0
  };

struct Lisp_Overlay * OVERLAY_SENTINEL = &OVERLAY_SENTINEL_NODE;

/* This function determines where overlays are inserted in the tree.
   FIXME: I didn't think too hard about this...
*/
static inline bool
overlay_lt (struct Lisp_Overlay *a, struct Lisp_Overlay *b)
{
  if (a->char_start < b->char_start)
    return true;
  else if (a->char_start > b->char_start)
    return false;

  if (a->char_end < b->char_end)
    return true;
  else if (a->char_end > b->char_end)
    return false;
  else
    return a < b;
}

/* Assert that OV has a parent of an admissable type.  */
#define EASSERT_PARENT_TYPE(ov)                  \
  if ((ov) != OVERLAY_SENTINEL)                  \
    do {                                         \
      eassert(OVERLAYP ((ov)->parent) ||         \
              BUFFERP ((ov)->parent)  ||         \
              NILP ((ov)->parent));              \
  } while (false)

#ifdef CHECK_OVERLAY_TREE
void
check_parents (struct Lisp_Overlay *root, Lisp_Object parent)
{
  if (root == OVERLAY_SENTINEL)
    return;

  /* These are there to print with `pp' in gdb */
  Lisp_Object me, left, right;
  XSETMISC (me, root);
  XSETMISC (left, root->left);
  XSETMISC (right, root->right);

  /* Check we have the correct parent */
  eassert ( EQ (root->parent, parent));

  check_parents(root->left, me);
  check_parents(root->right, me);
}

void
check_valid_aa_tree (struct Lisp_Overlay *root)
{

  if (root == OVERLAY_SENTINEL)
    return;


  /* Check it doesn't end before it starts */
  eassert (root->char_end >= root->char_start);
  eassert (root->level >= root->right->level &&
           root->level >= root->left->level);
  /* No horizontal left links */
  eassert (root->left->level < root->level);
  /* At most one link on same level */
  eassert (root->right->right->level < root->level);

  /* Check the max field */
  eassert (root->max >= root->left->max &&
           root->max >= root->left->max &&
           root->max >= root->char_end);
  /* Check subtrees */
  check_valid_aa_tree (root->left);
  check_valid_aa_tree (root->right);
}

void
check_tree_consistency (struct Lisp_Overlay *tree, Lisp_Object parent)
{
  check_valid_aa_tree(tree);
  check_parents(tree, parent);
}
#endif

/* Rebalancing.  See Andersson's paper for a good explaination.  This
   is a bit more complicated than his code, since we need to maintain
   parent pointer and max field.  */
inline static void
overlay_skew (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  EASSERT_PARENT_TYPE(t);
  if (t != OVERLAY_SENTINEL && t->left->level == t->level)
    {
      struct Lisp_Overlay *tmp = t;
      printf("skewing around %p\n", t);
      t = *tt = t->left;
      tmp->left = t->right;
      t->right = tmp;

      tmp->max = overlay_find_max(tmp);
      t->max = overlay_find_max(t);

      EASSERT_PARENT_TYPE(tmp);

      t->parent = tmp->parent;
      if (BUFFERP (t->parent))
        XBUFFER (t->parent)->overlays_root = t;
      XSETMISC (tmp->parent, t);
      eassert (OVERLAYP (tmp->parent));
      if (tmp->left != OVERLAY_SENTINEL)
        {
          /* eassert (OVERLAYP (tmp->left)); */
          /* eassert (OVERLAYP (tmp)); */
          eassert (OVERLAYP (tmp->left->parent));
          XSETMISC (tmp->left->parent, tmp);
        }
    }
}

/* Rebalancing.  See Andersson's paper for a good explaination.  This
   is a bit more complicated than his code, since we need to maintain
   parent pointer and max field.  */
inline static void
overlay_split (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  /* eassert (OVERLAYP (t->parent) || BUFFERP (t->parent)); */
  if (t != OVERLAY_SENTINEL && t->level == t->right->right->level )
    {
      printf("splitting around %p\n", t);
      struct Lisp_Overlay *tmp = t;
      t = *tt = t->right;
      tmp->right = t->left;
      if (tmp->right != OVERLAY_SENTINEL)
        XSETMISC (tmp->right->parent, tmp);
      t->left = tmp;
      t->level++;
      eassert (t->level - 1 == tmp->level);

      tmp->max = overlay_find_max(tmp);
      t->max = overlay_find_max(t);

      eassert (OVERLAYP (tmp->parent) || BUFFERP (tmp->parent));
      t->parent = tmp->parent;
      if (BUFFERP (t->parent))
        XBUFFER (t->parent)->overlays_root = t;
      XSETMISC (tmp->parent, t);
    }
}

/* Insert NODE in TREE.  When it is inserted, set its parent to
   PARENT.  On the way up after the insertion, adjust the max field of
   each node if needed.  */
static ptrdiff_t
overlay_tree_insert_internal (struct Lisp_Overlay **tree,
                              Lisp_Object parent,
                              struct Lisp_Overlay *node)
{
  Lisp_Object new_parent;
  eassert (OVERLAYP (parent) || BUFFERP (parent));
  struct Lisp_Overlay *t = *tree;
  /* eassert (OVERLAYP (t->parent) || BUFFERP (t->parent));   */

  if (t == OVERLAY_SENTINEL)
    {
      /* printf("Inserting overlay from %d to %d with parent %d at %p\n", */
             /* node->char_start, node->char_end, parent, node); */
      node->left = node->right = OVERLAY_SENTINEL;
      node->level = 1;
      node->max = node->char_end;
      /* XSETMISC (node->parent, parent); */
      node->parent = parent;
      eassert (OVERLAYP (node->parent) || BUFFERP (node->parent));
      if (BUFFERP (node->parent))
        XBUFFER (node->parent)->overlays_root = node;
      /* eassert (node->parent != OVERLAY_SENTINEL); */
      *tree = node;
      return node->max;
    }
  else
    {
      EASSERT_PARENT_TYPE(t);
      struct Lisp_Overlay **dir = overlay_lt (node, t) ?
        &t->left : &t->right;

      XSETMISC (new_parent, t);
      eassert (OVERLAYP (new_parent));
      ptrdiff_t child_max = overlay_tree_insert_internal(dir, new_parent, node);
      if (child_max > t->max)
        t->max = child_max;
    }
  CHECK_PARENTS(*tree, parent);
  overlay_skew (tree);
  CHECK_PARENTS(*tree, parent);
  overlay_split (tree);
  CHECK_PARENTS(*tree, parent);

  return t->max;
}

/* Insert NODE into TREE.  */
void
overlay_tree_insert (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node,
                     Lisp_Object buffer)
{
  CHECK_TREE_CONSISTENCY(*tree, buffer);
  overlay_tree_insert_internal(tree, buffer, node);
  CHECK_TREE_CONSISTENCY(*tree, buffer);
}

inline static void
replace_child (struct Lisp_Overlay *old, struct Lisp_Overlay *new)
{
  Lisp_Object old_parent = old->parent;
  if (BUFFERP (old_parent))
    {
      struct buffer *b = XBUFFER (old_parent);
      b->overlays_root = new;
    }
  else
    {
      eassert (OVERLAYP (old_parent));
      struct Lisp_Overlay *up = XOVERLAY (old_parent);
      printf("up is %p\n", up);
      eassert (up->left == old || up->right == old);
      if (up->left == old)
        {
          printf("set up's (%p) left child to %p\n", up, new);
          up->left = new;
        }
      else
        {
          printf("set up's (%p) left child to %p\n", up, new);
          up->right = new;
        }
    }

  if (new != OVERLAY_SENTINEL)
    {
      printf("Setting parent of %p to %ul\n", new, old_parent);
      new->parent = old_parent;
    }
}


/* Delete NODE from TREE.  */
void
overlay_tree_delete (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node)
{
  struct Lisp_Overlay *t = *tree;
  static __thread struct Lisp_Overlay *last, *deleted;

  if (t == OVERLAY_SENTINEL)
    {
      printf("hit the overlay sentinel\n");
    return;
    }

  last = t;
  if (overlay_lt(node, t))
    {
      printf("Turning left at %p\n", t);
      overlay_tree_delete (&t->left, node);
    }
  else
    {
      printf("Turning right at %p\n", t);
      deleted = t;
      overlay_tree_delete (&t->right, node);
    }

  if (t == last &&
      deleted != OVERLAY_SENTINEL &&
      node == deleted)
    {
      printf("Deleted overlay from %d to %d at addr: %p with level %d\n",
             deleted->char_start, deleted->char_end, deleted, deleted->level);
      if (last == deleted) printf("It was a leaf\n");
      printf("Its parent is %d\n", deleted->parent);

      bool is_leaf = (last == deleted);

      /* First handle the parent of DELETED.  */
      if (is_leaf)
        replace_child(deleted, OVERLAY_SENTINEL);
      else
        {
          replace_child(last, OVERLAY_SENTINEL);
          replace_child(deleted, last);
        }

      /* if (BUFFERP (deleted->parent)) */
      /*   { */
      /*     struct buffer *b = XBUFFER (deleted->parent); */
      /*     if (is_leaf) */
      /*       b->overlays_root = OVERLAY_SENTINEL; */
      /*     else */
      /*       { */
      /*         last->parent = deleted->parent; */
      /*         b->overlays_root = last; */
      /*       } */
      /*   } */
      /* else */
      /*   { */
      /*     eassert (OVERLAYP (deleted->parent)); */
      /*     struct Lisp_Overlay *up = XOVERLAY (deleted->parent); */
      /*     printf("up is %p\n", up); */
      /*     eassert (up != last); */
      /*     eassert (up->left == deleted || up->right == deleted); */
      /*     if (up->left == deleted) */
      /*       { */
      /*         printf("set up's (%p) left child to %p\n", up, last); */
      /*         up->left = is_leaf ? OVERLAY_SENTINEL : last; */
      /*       } */
      /*     else */
      /*       { */
      /*         printf("set up's (%p) left child to %p\n", up, last); */
      /*         up->right = is_leaf ? OVERLAY_SENTINEL : last; */
      /*       } */

      /*     if (! is_leaf) */
      /*       XSETMISC (last->parent, up); */
      /*   } */

      /* Then handle the children of LAST.  */
      if (! is_leaf)
        {
          eassert (deleted->left != last);
          printf("put last (%p), which had level %d, at level %d, like deleted (%p)\n",
                 last, last->level, deleted->level, deleted);
          last->level = deleted->level;
          last->left = deleted->left;
          if (last->left != OVERLAY_SENTINEL)
            XSETMISC (last->left->parent, last);

          if (deleted->right == last)
            last->right = OVERLAY_SENTINEL;
          else
            {
              eassert (last->right->level <= last->level);
              last->right = deleted->right;
              eassert (last->right->level <= last->level);
              if (last->right != OVERLAY_SENTINEL)
                XSETMISC (last->right->parent, last);
            }
        }

      deleted->parent = Qnil;
      deleted->right = OVERLAY_SENTINEL;
      deleted->left = OVERLAY_SENTINEL;
      deleted->deleted = 1;
    }
  else
    {
      /* Adjust all max fields along the path up.  */
      if (t == deleted)
        {
          printf("Hit deleted (%p) on the way up!\n", deleted);
          printf("Replacing deleted with last\n");
          t = *tree = last;
        }

      printf("Going up, at %p which has level %d\n", t, t->level);
      t->max = overlay_find_max(t);
      if (t->left->level < t->level - 1
          || t->right->level < t->level - 1)
        {
          t->level--;
          printf("lowering t (%p) to %d, right is at %d, left is at %d\n",
                 t, t->level, t->right->level, t->left->level);
          if (t->right->level > t->level)
            t->right->level = t->level;

          /* Andersson leaves it as 'an exercise for the reader' to
             prove that these rebalancing operations are enough.
             Don't you just love when that happens?  */
          struct Lisp_Overlay *c;
          c = *tree;
          printf("c=%d, c->r=%d, c->r->r=%d\n", c->level,
                 c->right->level, c->right->right->level);
          eassert (c->level >= c->right->level &&
                   c->level >= c->right->level);
          overlay_skew (tree);

          c = (*tree)->right;
          printf("c=%d, c->r=%d, c->r->r=%d\n", c->level,
                 c->right->level, c->right->right->level);
          eassert (c->level >= c->right->level &&
                   c->level >= c->right->level);
          overlay_skew (&(*tree)->right);

          c = (*tree)->right->right;
          printf("c=%d, c->r=%d, c->r->r=%d\n", c->level,
                 c->right->level, c->right->right->level);
          eassert (c->level >= c->right->level &&
                   c->level >= c->right->level);
          overlay_skew (&(*tree)->right->right);

          c = *tree;
          printf("c=%d, c->r=%d, c->r->r=%d\n", c->level,
                 c->right->level, c->right->right->level);
          eassert (c->level >= c->right->level &&
                   c->level >= c->right->level);
          overlay_split (tree);

          c = (*tree)->right;
          printf("c=%d, c->r=%d, c->r->r=%d\n", c->level,
                 c->right->level, c->right->right->level);
          eassert (c->level >= c->right->level &&
                   c->level >= c->right->level);
          overlay_split (&(*tree)->right);
        }
    }
}

static void
add_to_vec (struct Lisp_Overlay *elm, Lisp_Object **vecp,
            ptrdiff_t *size, ptrdiff_t *idx)
{
  if (*idx == *size - 1)
    {
      *size += 50;
      *vecp = xnrealloc (*vecp, *size, sizeof (Lisp_Object));
    }

  XSETMISC((*vecp)[(*idx)++], elm);
}

void
overlay_tree_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 ptrdiff_t *idx)
{
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  if (tree->char_start <= pos)
    {
      if (tree->char_end > pos)
      add_to_vec(tree, vec_ptr, vec_size, idx);
      overlay_tree_at (tree->right, pos, vec_size, vec_ptr, idx);
    }
  overlay_tree_at (tree->left, pos, vec_size, vec_ptr, idx);
}

void
overlay_tree_in (struct Lisp_Overlay *tree, ptrdiff_t beg,
                 ptrdiff_t end, ptrdiff_t buf_end,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 ptrdiff_t *idx)
{
  if (tree == OVERLAY_SENTINEL || tree->max < beg)
    return;

  if ((tree->char_start < end && tree->char_end > beg)
      || (tree->char_start == tree->char_end &&
          (tree->char_start == beg ||
           (tree->char_start == end && end == buf_end))))
    add_to_vec(tree, vec_ptr, vec_size, idx);

  if (tree->char_start <= beg)
    overlay_tree_in (tree->right, beg, end, buf_end,
                     vec_size, vec_ptr, idx);
  overlay_tree_in (tree->left, beg, end, buf_end,
                   vec_size, vec_ptr, idx);
}


static void
overlay_tree_drop_all_internal (struct buffer *buf,
                                struct Lisp_Overlay *tree)
{
  if (tree == OVERLAY_SENTINEL)
    return;
  overlay_tree_drop_all_internal (buf, tree->left);
  overlay_tree_drop_all_internal (buf, tree->right);
  modify_overlay (buf, tree->char_start, tree->char_end);
}

void
overlay_tree_next_change (struct Lisp_Overlay *tree,
                          ptrdiff_t pos, ptrdiff_t *best)
{
  ptrdiff_t startdiff, enddiff;
  eassert (pos >= 1);
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  if (tree->char_start > pos)
    {
      startdiff = tree->char_start - pos;
      if (startdiff < *best)
        *best = startdiff;
    }
  else
    overlay_tree_next_change (tree->right, pos, best);

  if (tree->char_end > pos)
    {
      enddiff = tree->char_end - pos;
      if (enddiff < *best)
        *best = enddiff;
    }
  overlay_tree_next_change (tree->left, pos, best);
}

void
overlay_tree_prev_change (struct Lisp_Overlay *tree,
                          ptrdiff_t pos, ptrdiff_t *best)
{
  ptrdiff_t startdiff, enddiff;
  eassert (pos >= 1);
  if (tree == OVERLAY_SENTINEL)
    return;

  if (tree->char_end < pos)
    {
      enddiff = pos - tree->char_end;
      if (enddiff < *best)
        *best = enddiff;
    }
  overlay_tree_prev_change (tree->left, pos, best);

  if (tree->char_start < pos)
    {
      startdiff = pos - tree->char_start;
      if (startdiff < *best)
        *best = startdiff;
      overlay_tree_prev_change (tree->right, pos, best);
    }
}


void
overlay_tree_drop_all(struct buffer *buf)
{
  overlay_tree_drop_all_internal (buf, buf->overlays_root);
  buf->overlays_root = OVERLAY_SENTINEL;
}

/* Add all nodes in TREE to VEC_PTR, which has size VEC_SIZE, starting
   from IDX.  The nodes will be added in the order they have in the
   tree.  */
static void
overlay_tree_all_internal (struct Lisp_Overlay *tree,
                           ptrdiff_t *vec_size,
                           Lisp_Object **vec_ptr,
                           ptrdiff_t *idx)
{
  if (tree == OVERLAY_SENTINEL)
    return;

  overlay_tree_all_internal (tree->left, vec_size,
                             vec_ptr, idx);
  /* add_to_vec (vec_size, vec_ptr, idx, tree); */
  overlay_tree_all_internal (tree->right, vec_size,
                             vec_ptr, idx);
}

/* Put all nodes from TREE into VEC_PTR, adjusting VEC_SIZE as
   necessary.  */
ptrdiff_t
overlay_tree_all (struct Lisp_Overlay *tree, ptrdiff_t *vec_size,
                  Lisp_Object **vec_ptr)
{
  ptrdiff_t n = 0;
  overlay_tree_all_internal (tree, vec_size, vec_ptr, &n);
  return n;
}

/* Return the buffer OVERLAY is in or nil if OVERLAY has been
   deleted. */
Lisp_Object
buffer_of_overlay (Lisp_Object overlay)
{
  Lisp_Object o = overlay;
  while (!NILP (XOVERLAY (o)->parent) &&
         !BUFFERP (XOVERLAY (o)->parent))
    {
      eassert (OVERLAYP (XOVERLAY (o)->parent));
      eassert (XOVERLAY (XOVERLAY (o)->parent) != XOVERLAY (o));
      o = XOVERLAY (o)->parent;
    }
  return XOVERLAY (o)->parent;
}

void
overlay_tree_zero_size_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                            Lisp_Object hit_list)
{
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  if (tree->char_start == pos && tree->char_end == pos)
    {
      Lisp_Object ov;
      XSETMISC (ov, tree);
      Fcons(hit_list, ov);
    }

  overlay_tree_zero_size_at (tree->right, pos, hit_list);
  if (pos >= tree->char_start)
    overlay_tree_zero_size_at (tree->left, pos, hit_list);
}

/* Adjust CHARPOS asn BYTEPOS for an insert from FROM_CHAR
   to TO_CHAR.
   FIXME: insertion_type and before.
 */
static void
adjust_pos_for_insert (ptrdiff_t *charpos, ptrdiff_t from_char,
                       ptrdiff_t to_char, bool insertion_type,
                       bool before)
{
  if (*charpos > from_char)
    {
      *charpos += to_char - from_char;
    }
  else if (*charpos == from_char && insertion_type)
    {
      *charpos = to_char;
    }
}

/* Adjust all nodes in TREE for an insert from FROM_CHAR (FROM_BYTE)
   to TO_CHAR (TO_BYTE).  Return TREEs max.
   FIXME: before.  */
ptrdiff_t
overlay_tree_adjust_for_insert (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char,
                                bool before)
{
  /* If we are at a leaf or all nodes in TREE are before the insert,
   return.  */
  if (tree == OVERLAY_SENTINEL || tree->max < from_char)
    return tree->max;

  /* Adjust the start postions.  */
  adjust_pos_for_insert(&tree->char_start, from_char, to_char,
                        tree->start_insertion_type, before);
  /* Adjust the end postions.  */
  adjust_pos_for_insert(&tree->char_end, from_char, to_char,
                        tree->end_insertion_type, before);

  ptrdiff_t r,l;

  l = overlay_tree_adjust_for_insert (tree->left, from_char,
                                      to_char, before);
  r = overlay_tree_adjust_for_insert (tree->right, from_char,
                                      to_char, before);

  tree->max = overlay_max(l, r, tree->char_end);
  return tree->max;
}

/* Adjust CHARPOS for a delete from FROM_CHAR to TO_CHAR.  */
static void
adjust_pos_for_delete (ptrdiff_t *charpos, ptrdiff_t from_char,
                       ptrdiff_t to_char)
{
  if (*charpos > to_char)
    {
      *charpos = to_char - from_char;
    }
  else if (*charpos > from_char)
    {
      *charpos = from_char;
    }
}

/* Adjust TREE for a delete from FROM_CHAR to TO_CHAR.  */
ptrdiff_t
overlay_tree_adjust_for_delete (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char)
{
  if (tree == OVERLAY_SENTINEL || tree->max < from_char)
    return tree->max;

  adjust_pos_for_delete(&tree->char_start, from_char, to_char);
  adjust_pos_for_delete(&tree->char_end, from_char, to_char);

  ptrdiff_t r, l;

  l = overlay_tree_adjust_for_delete(tree->left, from_char, to_char);
  r = overlay_tree_adjust_for_delete(tree->right, from_char, to_char);

  tree->max = overlay_max(l, r, tree->char_end);
  return tree->max;
}

/* Adjust CHARPOS and BYTEPOS for a replace from FROM_CHAR to TO_CHAR.
 */
static void
adjust_pos_for_replace (ptrdiff_t *charpos, ptrdiff_t from_char,
                        ptrdiff_t old_chars, ptrdiff_t new_chars)
{
  ptrdiff_t diff_chars = new_chars - old_chars;

  if (*charpos >= (from_char + old_chars))
    {
      *charpos += diff_chars;
    }
  else if (*charpos > from_char)
    {
      *charpos = from_char;
    }
}

/* Adjust TREE for a delete from FROM_CHAR to TO_CHAR.  */
ptrdiff_t
overlay_tree_adjust_for_replace (struct Lisp_Overlay *tree,
                                 ptrdiff_t from_char,
                                 ptrdiff_t old_chars,
                                 ptrdiff_t new_chars)
{
  if (tree == OVERLAY_SENTINEL || tree->max <= from_char)
    return tree->max;

  adjust_pos_for_replace(&tree->char_start, from_char,
                         old_chars, new_chars);
  adjust_pos_for_replace(&tree->char_end, from_char,
                         old_chars, new_chars);

  ptrdiff_t r, l;

  l = overlay_tree_adjust_for_replace(tree->left, from_char,
                                      old_chars, new_chars);
  r = overlay_tree_adjust_for_replace(tree->right, from_char,
                                      old_chars, new_chars);

  tree->max = overlay_max(l, r, tree->char_end);
  return tree->max;
}

/* These are for debugging */

DEFUN("overlays-root", Foverlays_root, Soverlays_root, 0, 1, 0,
      doc: /* Root of the overlays tree of BUFFER or current buffer. */)
  (Lisp_Object buffer)
{
  Lisp_Object ret;
  struct buffer *buf;
  if (NILP (buffer))
    buf = current_buffer;
  else
    buf = XBUFFER (buffer);

  if (buf->overlays_root == OVERLAY_SENTINEL)
    return Qnil;

  XSETMISC (ret, buf->overlays_root);
  return ret;
}

DEFUN("overlay-parent", Foverlay_parent, Soverlay_parent, 1, 1, 0,
      doc: /* Parent of overlay.  An overlay or a buffer.  */)
  (Lisp_Object overlay)
{
  if (!OVERLAYP(overlay))
    signal_error("Not an overlay", Qnil);
  return XOVERLAY (overlay)->parent;
}

DEFUN("overlay-left", Foverlay_left, Soverlay_left, 1, 1, 0,
      doc: /* Left child of overlay.  */)
  (Lisp_Object overlay)
{
  Lisp_Object o;
  XSETMISC (o, XOVERLAY (overlay)->left);
  return o;
}

DEFUN("overlay-right", Foverlay_right, Soverlay_right, 1, 1, 0,
      doc: /* Right child of overlay.  */)
  (Lisp_Object overlay)
{
  Lisp_Object o;
  XSETMISC (o, XOVERLAY (overlay)->right);
  return o;
}

DEFUN("overlay-info", Foverlay_info, Soverlay_info, 1, 1, 0,
      doc: /* Info about OVERLAY.  */)
  (Lisp_Object overlay)
{
  if (!OVERLAYP(overlay))
    signal_error("Not an overlay", Qnil);
  Lisp_Object ret;
  struct Lisp_Overlay *o = XOVERLAY (overlay);
  Lisp_Object left, right, this;
  if (o->left != OVERLAY_SENTINEL)
    XSETMISC(left, o->left);
  else
    left = Qt;

  if (o->right != OVERLAY_SENTINEL)
    XSETMISC(right, o->right);
  else
    right = Qt;

  XSETMISC (this, o);

  ret = list5(Fcons(Fcons(make_number(o->char_start),
                          make_number(o->char_end)),
                    make_number(o->max)),
              this,
              o->parent,
              make_number (o->level),
              Fcons(left,
                    right));
  return ret;
}

DEFUN("overlays-in-buffer", Foverlays_in_buffer, Soverlays_in_buffer,
      0, 1, 0,
      doc: /* Return a list of all the overlays in BUFFER.  */)
  (Lisp_Object buffer)
{
  Lisp_Object ret;
  struct buffer *b;
  if (!NILP (buffer))
    b = XBUFFER (buffer);
  else
    b = current_buffer;


  ptrdiff_t vec_size = 30;
  Lisp_Object *vec = xnmalloc (vec_size, sizeof (Lisp_Object));


  ptrdiff_t noverlays = overlay_tree_all(b->overlays_root,
                                         &vec_size, &vec);
  ret = Flist(noverlays, vec);

  return ret;
}

DEFUN("overlay-subtree", Foverlay_subtree, Soverlay_subtree, 1, 1, 0,
      doc: /* Return the subtree rooted at overlay as a list.  Will
              not work for for the sentinel.  */)
  (Lisp_Object overlay)
{
  Lisp_Object ret, left, right, lefttree, righttree;
  eassert (OVERLAYP (overlay));
  struct Lisp_Overlay *me = XOVERLAY (overlay);

  if (me->left == OVERLAY_SENTINEL)
    lefttree = Qnil;
  else
    {
      XSETMISC (left, me->left);
      lefttree = Fcons (Foverlay_subtree(left), Qnil);
    }

  if (me->right == OVERLAY_SENTINEL)
    righttree = Qnil;
  else
    {
      XSETMISC (right, me->right);
      righttree = Fcons (Foverlay_subtree (right), Qnil);
    }


  ret = Fcons (overlay,
               Fcons (lefttree,
                      Fcons (righttree, Qnil)));
  return ret;
}

DEFUN("overlay-tree", Foverlay_tree, Soverlay_tree, 0, 1, 0,
      doc: /* Return the overlay tree of BUFFER, as a list.  */)
  (Lisp_Object buffer)
{
  Lisp_Object ret, ov;
  struct buffer *b;
  if (!NILP (buffer))
    b = XBUFFER (buffer);
  else
    b = current_buffer;

  if (b->overlays_root == OVERLAY_SENTINEL)
    return Qnil;

  XSETMISC (ov, b->overlays_root);
  ret = Foverlay_subtree(ov);

  return ret;
}

DEFUN("overlay-tree-at", Foverlay_tree_at, Soverlay_tree_at,
      1, 2, 0,
      doc: /* Return a list of all overlays in BUFFER.  If BUFFER is
      nil, use the current buffer.  */)
  (Lisp_Object pos, Lisp_Object buffer)
{
  CHECK_NUMBER_COERCE_MARKER (pos);

  ptrdiff_t next, prev;
  ptrdiff_t bufpos = XINT (pos);

  Lisp_Object ret;
  struct buffer *b;
  if (!NILP (buffer))
    b = XBUFFER (buffer);
  else
    b = current_buffer;


  ptrdiff_t vec_size = 30;
  Lisp_Object *vec = xnmalloc (vec_size, sizeof (Lisp_Object));


  /* ptrdiff_t noverlays = overlay_tree_at(b->overlays_root, bufpos, */
  /*                                       &next, &prev, */
  /*                                       &vec_size, &vec, false); */

  /* ret = Flist(noverlays, vec); */

  /* return ret; */
  return 0;
}

void
syms_of_overlays (void)
{
  defsubr (&Soverlay_parent);
  defsubr (&Soverlay_info);
  defsubr (&Soverlays_in_buffer);
  defsubr (&Soverlay_tree_at);
  defsubr (&Soverlay_right);
  defsubr (&Soverlay_left);
  defsubr (&Soverlays_root);
  defsubr (&Soverlay_tree);
  defsubr (&Soverlay_subtree);
}
