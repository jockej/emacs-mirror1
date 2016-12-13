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
#include <stdio.h>

/* Defined in buffer.c */
extern void
modify_overlay (struct buffer *, ptrdiff_t, ptrdiff_t);

extern bool
lookup_char_property (Lisp_Object, Lisp_Object, Lisp_Object);

/* Return the max of a, b and c.  */
static inline ptrdiff_t
overlay_max (ptrdiff_t a, ptrdiff_t b, ptrdiff_t c)
{
  ptrdiff_t bc_max = max (b, c);
  return max (a, bc_max);
}

/* Find what the max value should be for X.  */
static inline ptrdiff_t
overlay_find_max (struct Lisp_Overlay *x)
{
  return overlay_max (x->left->max, x->right->max, x->char_end);
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
    .char_start = 0,
    .char_end = 0
    /* .parent = 0 */
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


#ifdef CHECK_OVERLAY_TREE
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

  /* Check we don't have ourselves as a child */
  eassert (root->left != root && root->right != root);

  /* Check the max field */
  eassert (root->max >= root->left->max &&
           root->max >= root->right->max &&
           root->max >= root->char_end);
  /* Check subtrees */
  check_valid_aa_tree (root->left);
  check_valid_aa_tree (root->right);
}
#endif

/* Rebalancing.  See Andersson's paper for a good explaination .  */
inline static void
overlay_skew (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  if (t != OVERLAY_SENTINEL && t->left->level == t->level)
    {
      struct Lisp_Overlay *tmp = t;
      /* printf("skewing around %p\n", t); */
      /* printf("skew: t->max=%li, t->left->max=%li, t->right->max=%li\n" */
             /* ,t->max, t->left->max, t->right->max); */
      /* printf("skew: tmp->max=%li, tmp->left->max=%li, tmp->right->max=%li\n" */
             /* ,tmp->max, tmp->left->max, tmp->right->max); */
      t = *tt = t->left;
      tmp->left = t->right;
      t->right = tmp;
      /* printf("skew: tmp->max=%li, t->max=%li\n", tmp->max, t->max); */
      tmp->max = overlay_find_max(tmp);
      t->max = overlay_find_max(t);
      /* printf("skew: tmp->max=%li, t->max=%li\n", tmp->max, t->max); */
    }
}

/* Rebalancing.  See Andersson's paper for a good explaination.  This
   is a bit more complicated than his code, since we need to maintain
   parent pointer and max field.  */
inline static void
overlay_split (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  if (t != OVERLAY_SENTINEL && t->level == t->right->right->level )
    {
      /* printf("splitting around %p\n", t); */
      struct Lisp_Overlay *tmp = t;
      t = *tt = t->right;
      tmp->right = t->left;
      t->left = tmp;
      t->level++;
      eassert (t->level - 1 == tmp->level);

      tmp->max = overlay_find_max(tmp);
      t->max = overlay_find_max(t);
    }
}

void
print_tree(struct Lisp_Overlay *tree, unsigned level)
{

  for (unsigned i = 0; i < level; i++)
    printf("  ");
  if (tree == OVERLAY_SENTINEL)
    {
      printf("nil\n");
      return;
    }
  printf("%li to %li\n", tree->char_start, tree->char_end);
  print_tree(tree->right, level + 2);
  print_tree(tree->left, level + 2);
}

/* Insert NODE in TREE.  On the way up after the insertion, adjust the
   max field of each node if needed.  */
static ptrdiff_t
overlay_tree_insert_internal (struct Lisp_Overlay **tree,
                              struct Lisp_Overlay *node)
{
  struct Lisp_Overlay *t = *tree;

  if (t == OVERLAY_SENTINEL)
    {
      /* printf("Inserting overlay from %li to %li with max %li at %p\n", */
             /* node->char_start, node->char_end, node->char_end, node); */
      node->left = node->right = OVERLAY_SENTINEL;
      node->level = 1;
      node->max = node->char_end;

      *tree = node;
      return node->max;
    }
  else
    {
      struct Lisp_Overlay **dir = overlay_lt (node, t) ? &t->left : &t->right;

      /* if (dir == &t->left) */
        /* printf("insert: going left at %p\n", t); */
      /* else if (dir == &t->right) */
        /* printf("insert: going right at %p\n", t); */
      ptrdiff_t child_max = overlay_tree_insert_internal (dir, node);
      /* printf("(t = %p) t->max: %li, child_max: %li\n", t, t->max, child_max); */
      if (child_max > t->max)
        {
          t->max = child_max;
        }
    }
  /* printf("t=%p, *tree=%p\n", t, *tree); */
  overlay_skew (tree);
  overlay_split (tree);
  /* printf("t=%p, *tree=%p\n", t, *tree); */

  /* printf("insert internal: (*tree)->max=%li, t->max=%li\n", */
         /* (*tree)->max, t->max); */
  return (*tree)->max;
}

/* Insert NODE into TREE.  */
void
overlay_tree_insert (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node)
{
  CHECK_TREE (*tree);
  overlay_tree_insert_internal(tree, node);
  CHECK_TREE (*tree);
}

/* Delete NODE from TREE.  */
void
overlay_tree_delete (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node,
                     struct Lisp_Overlay *parent)
{
  struct Lisp_Overlay *t = *tree;
  static __thread struct Lisp_Overlay *last, *lasts_parent,
    *deleted, *deleteds_parent;

  /* printf("top of delete: tree=%p, *tree=%p\n", tree, *tree); */
  if (t == OVERLAY_SENTINEL)
    {
      /* printf("hit the overlay sentinel\n"); */
      return;
    }

  last = t;
  lasts_parent = parent;
  if (overlay_lt(node, t))
    {
      /* printf("Turning left at %p, sending in %p as tree\n", t, &t->left); */
      overlay_tree_delete (&t->left, node, t);
    }
  else
    {
      /* printf("Turning right at %p, sending in %p as tree\n", t, &t->right); */
      deleted = t;
      deleteds_parent = parent;
      overlay_tree_delete (&t->right, node, t);
    }

  if (t == last &&
      deleted != OVERLAY_SENTINEL &&
      node == deleted)
    {
      /* printf("Deleted overlay from %ld to %ld at addr: %p with level %u and parent %p\n", */
             /* deleted->char_start, deleted->char_end, deleted, */
             /* deleted->level, deleteds_parent); */
      /* if (last == deleted) printf("It was a leaf\n"); */

      if (last != deleted)
        {
          eassert (deleted->left != last);
          /* printf("put last (%p), which had level %u, at level %u, like deleted (%p)\n", */
                 /* last, last->level, deleted->level, deleted); */
          last->level = deleted->level;
          last->left = deleted->left;

          if (deleted->right == last)
            ;            /* last->right = OVERLAY_SENTINEL; */
          else
            {
              eassert (last->right->level <= last->level);
              last->right = deleted->right;
              eassert (last->right->level <= last->level);
            }
        }

      if (!parent)
        {
          /* printf("deleted (%p) had NULL parent, setting it's buffer's root to OVERLAY_SENTINEL\n", deleted); */
          XBUFFER (deleted->buf)->overlays_root = OVERLAY_SENTINEL;
        }

      deleted->buf = Qnil;
      //deleted->right = OVERLAY_SENTINEL;
      //deleted->left = OVERLAY_SENTINEL;
      deleted->deleted = 1;
    }
  else
    {
      /* printf("going up: %li to %li\n", t->char_start, t->char_end); */
      /* PRINT_TREE(current_buffer->overlays_root); */
      if (t == lasts_parent)
        {
          if (t->left == last)
            {
              /* printf("last was left child of t (%p), putting OVERLAY_SENTINEL as child at %p (before tree is %p)\n", t, &t->left, tree); */
              eassert (t == lasts_parent || lasts_parent == deleted);
              t->left = OVERLAY_SENTINEL;
              /* printf("tree is now %p\n", tree); */
            }
          else if (t->right == last)
            {
              /* printf("last was right child of t (%p), putting OVERLAY_SENTINEL as child at %p (before tree is %p)\n", t, &t->right, tree); */
              eassert (t == lasts_parent || lasts_parent == deleted);
              t->right = OVERLAY_SENTINEL;
              /* printf("tree is now %p\n", tree); */
            }
        }

      if (t->left == deleted)
        {
          /* printf("deleted was left child of t (%p), putting last (%p) as child\n", t, last); */
          eassert (t == deleteds_parent);
          t->left = last;
        }
      else if (t->right == deleted)
        {
          /* printf("deleted was right child of t (%p), putting last (%p) as child\n", t, last); */
          eassert (t == deleteds_parent);
          t->right = last;
        }

      if (t == deleted)
        {
          /* printf("Hit deleted (%p) on the way up!\n", deleted); */
          /* printf("Replacing deleted with last\n"); */
          t = *tree = last;
        }

      /* printf("going up: before lowering: %li to %li\n", t->char_start, t->char_end); */
      /* PRINT_TREE(current_buffer->overlays_root); */
      /* printf("Going up, at %p which has level %u\n", t, t->level); */
      /* Adjust all max fields along the path up.  */
      t->max = overlay_find_max(t);
      if (t->left->level < t->level - 1
          || t->right->level < t->level - 1)
        {
          t->level--;
          /* printf("lowering t (%p) to %u, right is at %u, left is at %u\n", */
                 /* t, t->level, t->right->level, t->left->level); */
          if (t->right->level > t->level)
            t->right->level = t->level;

          /* Andersson leaves it as 'an exercise for the reader' to
             prove that these rebalancing operations are enough.
             Don't you just love when that happens?  */
          /* struct Lisp_Overlay *c; */
          /* c = *tree; */
          /* printf("delete: t=%p, *tree=%p\n", t, *tree); */
          /* printf("c=%u, c->r=%u, c->r->r=%u\n", c->level, */
                 /* c->right->level, c->right->right->level); */
          /* eassert (c->level >= c->right->level && */
                   /* c->level >= c->left->level); */
          /* printf("before first skew:\n"); */
          /* PRINT_TREE(current_buffer->overlays_root); */
          overlay_skew (tree);
          /* printf("after first skew:\n"); */
          /* PRINT_TREE(current_buffer->overlays_root); */

          /* c = (*tree)->right; */
          /* printf("c=%u, c->r=%u, c->r->r=%u\n", c->level, */
                 /* c->right->level, c->right->right->level); */
          /* eassert (c->level >= c->right->level && */
                   /* c->level >= c->left->level); */
          /* printf("before 2nd skew:\n"); */
          /* PRINT_TREE(current_buffer->overlays_root); */
          overlay_skew (&(*tree)->right);
          /* printf("after 2nd skew:\n"); */
          /* PRINT_TREE(current_buffer->overlays_root); */

          /* c = (*tree)->right->right; */
          /* printf("c=%u, c->r=%u, c->r->r=%u\n", c->level, */
                 /* c->right->level, c->right->right->level); */
          /* eassert (c->level >= c->right->level && */
                   /* c->level >= c->left->level); */
          /* printf("before 3rd skew:\n"); */
          /* PRINT_TREE(current_buffer->overlays_root); */
          overlay_skew (&(*tree)->right->right);
          /* printf("after 3rd skew:\n"); */
          /* PRINT_TREE(current_buffer->overlays_root); */

          /* c = *tree; */
          /* printf("c=%u, c->r=%u, c->r->r=%u\n", c->level, */
                 /* c->right->level, c->right->right->level); */
          /* eassert (c->level >= c->left->level && */
                   /* c->level >= c->right->level); */
          overlay_split (tree);

          /* c = (*tree)->right; */
          /* printf("c=%u, c->r=%u, c->r->r=%u\n", c->level, */
                 /* c->right->level, c->right->right->level); */
          /* eassert (c->level >= c->right->level && */
                   /* c->level >= c->right->level); */
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
  CHECK_TREE (tree);
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  eassert (tree->left != tree && tree->right != tree);

  if (tree->char_start <= pos)
    {
      if (tree->char_end > pos)
      add_to_vec(tree, vec_ptr, vec_size, idx);
      overlay_tree_at (tree->right, pos, vec_size, vec_ptr, idx);
    }
  overlay_tree_at (tree->left, pos, vec_size, vec_ptr, idx);
}

/* This is exactly like `overlay_tree_at', except this also includes
   overlays whose end is >= POS.  */
void
overlay_tree_around (struct Lisp_Overlay *tree, ptrdiff_t pos,
                     ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                     ptrdiff_t *idx)
{
  CHECK_TREE (tree);
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  if (tree->char_start <= pos)
    {
      if (tree->char_end >= pos)
      add_to_vec(tree, vec_ptr, vec_size, idx);
      overlay_tree_around (tree->right, pos, vec_size, vec_ptr, idx);
    }
  overlay_tree_around (tree->left, pos, vec_size, vec_ptr, idx);
}

void
overlay_tree_endpoint_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                          struct window *w, ptrdiff_t *vec_size,
                          Lisp_Object **vec_ptr, ptrdiff_t *idx)
{
  CHECK_TREE (tree);
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  if (tree->char_start == pos || tree->char_end == pos)
    {
      Lisp_Object win =  lookup_char_property (tree->plist, Qwindow, 0);
      if (WINDOWP (win) && XWINDOW (win) == w)
        {
          add_to_vec (tree, vec_ptr, vec_size, idx);
        }
    }

  if (tree->char_start <= pos)
    overlay_tree_endpoint_at (tree->right, pos, w,
                              vec_size, vec_ptr, idx);
  overlay_tree_endpoint_at (tree->left, pos, w,
                            vec_size, vec_ptr, idx);
}


void
overlay_tree_evap (struct Lisp_Overlay *tree, ptrdiff_t pos,
                   ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                   ptrdiff_t *idx)
{
  CHECK_TREE (tree);
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  eassert (tree->left != tree && tree->right != tree);

  if (tree->char_start <= pos)
    {
      if (tree->char_end == pos && tree->char_start == pos &&
          ! NILP (lookup_char_property (tree->plist, Qevaporate, 0)))
        add_to_vec (tree, vec_ptr, vec_size, idx);
      overlay_tree_evap (tree->right, pos, vec_size, vec_ptr, idx);
    }
  overlay_tree_evap (tree->left, pos, vec_size, vec_ptr, idx);
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

struct Lisp_Overlay *
overlay_tree_next_start (struct Lisp_Overlay *tree, ptrdiff_t pos)
{
  if (tree == OVERLAY_SENTINEL)
    return tree;

  if (tree->char_start <= pos)
    {
      if (tree->right->char_start > pos)
        return tree->right;
      else
        return overlay_tree_next_start (tree->right, pos);
    }
  else
    return overlay_tree_next_start (tree->left, pos);
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
  add_to_vec (tree, vec_ptr, vec_size, idx);
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
  return XOVERLAY (overlay)->buf;
}

static void
reinsert_negative_size_overlays (struct Lisp_Overlay **root,
                                 Lisp_Object *vecp, ptrdiff_t n)
{
  struct Lisp_Overlay *o;
  for (ptrdiff_t i = 0; i < n; i++)
    {
      o = XOVERLAY (vecp[i]);
      eassert (o->char_end < o->char_start);
      o->char_end = o->char_start;
      CHECK_TREE (*root);
      overlay_tree_delete(root, o, NULL);
      CHECK_TREE (*root);
      overlay_tree_insert(root, o);
      CHECK_TREE (*root);
    }
}

/* Adjust CHARPOS for an insert from FROM_CHAR to TO_CHAR.  FIXME:
   insertion_type and before.
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
static ptrdiff_t
adjust_tree_for_insert (struct Lisp_Overlay *tree, ptrdiff_t from_char,
                        ptrdiff_t to_char, bool before,
                        Lisp_Object **neg_size, ptrdiff_t *len,
                        ptrdiff_t *idx)
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

  /* eassert (tree->char_start <= tree->char_end); */
  if (tree->char_end < tree->char_start)
    add_to_vec(tree, neg_size, len, idx);

  ptrdiff_t r,l;

  l = adjust_tree_for_insert (tree->left, from_char, to_char,
                              before, neg_size, len, idx);
  r = adjust_tree_for_insert (tree->right, from_char, to_char,
                              before, neg_size, len, idx);

  tree->max = overlay_max(l, r, tree->char_end);
  return tree->max;
}


/* Adjust all nodes in TREE for an insert from FROM_CHAR (FROM_BYTE)
   to TO_CHAR (TO_BYTE).  Return TREEs max.
   FIXME: before.  */
void
overlay_tree_adjust_for_insert (struct Lisp_Overlay **tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char,
                                bool before)
{
  Lisp_Object *neg_size;
  ptrdiff_t len = 30, idx = 0;

  neg_size = xnmalloc (len, sizeof(neg_size));
  adjust_tree_for_insert(*tree, from_char, to_char, before,
                         &neg_size, &len, &idx);

  if (idx > 0)
    reinsert_negative_size_overlays(tree, neg_size, idx);
  xfree (neg_size);
}

/* Adjust CHARPOS for a delete from FROM_CHAR to TO_CHAR.  */
static void
adjust_pos_for_delete (ptrdiff_t *charpos, ptrdiff_t from_char,
                       ptrdiff_t to_char)
{
  if (*charpos > to_char)
    {
      *charpos -= to_char - from_char;
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

  eassert (tree->char_start <= tree->char_end);
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
  eassert (tree->char_start <= tree->char_end);

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

/* DEFUN("overlay-parent", Foverlay_parent, Soverlay_parent, 1, 1, 0, */
/*       doc: /\* Parent of overlay.  An overlay or a buffer.  *\/) */
/*   (Lisp_Object overlay) */
/* { */
/*   if (!OVERLAYP(overlay)) */
/*     signal_error("Not an overlay", Qnil); */
/*   return XOVERLAY (overlay)->parent; */
/* } */

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

  ret = list4(Fcons(Fcons(make_number(o->char_start),
                          make_number(o->char_end)),
                    make_number(o->max)),
              this,
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

  xfree (vec);
  return ret;
}

DEFUN("overlay-subtree", Foverlay_subtree, Soverlay_subtree, 1, 1, 0,
      doc: /* Return the subtree rooted at OVERLAY as a list.  Will
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


void
syms_of_overlays (void)
{
  defsubr (&Soverlay_info);
  defsubr (&Soverlays_in_buffer);
  /* defsubr (&Soverlay_tree_at); */
  defsubr (&Soverlay_right);
  defsubr (&Soverlay_left);
  defsubr (&Soverlays_root);
  defsubr (&Soverlay_tree);
  defsubr (&Soverlay_subtree);
}
