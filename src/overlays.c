/*
 * Copyright (C) 2016  Joakim Jalap
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
   possible to the one presented in Anderssons paper (REF)[1], but
   with a few differences.  The code in [1] is written in Pascal,
   which has proper pass by reference. Unfortunately C doesn't really
   have this, so it has to be emulated.  This is the reason for
   passing around pointers to pointers all the time.

   Also the code in the delete routine is a bit more complicated
   because we need to delete the actual memory area, so as to not trip
   up the GC.  See comment above 'ot_delete'.

   The fact that this is an augmented tree also makes the rebalancing
   operation (split and skew) a bit more complex.

   [1] "Balanced Search Trees Made Simple", Arne Andersson, Workshop
   on Algorithms and Data Structures, pages 60-71, Springer Verlag, 1993
*/

#include "overlays.h"
#include "buffer.h"
#include "window.h"
#include <stdlib.h>

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
ot_find_max (struct Lisp_Overlay *x)
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
  };

struct Lisp_Overlay * OVERLAY_SENTINEL = &OVERLAY_SENTINEL_NODE;

/* This function determines where overlays are inserted in the tree.
 */
static inline bool
ot_lt (struct Lisp_Overlay *a, struct Lisp_Overlay *b)
{
  return a->char_start < b->char_start;
}


#ifdef CHECK_OVERLAY_TREE
void
check_valid_aa_tree (struct Lisp_Overlay *root, struct Lisp_Overlay *parent)
{

  if (root == OVERLAY_SENTINEL)
    return;

  eassert (!parent || (root == parent->right || root == parent->left));

  /* Check it doesn't end before it starts */
  eassert (root->char_end >= root->char_start);
  eassert (root->level >= root->right->level &&
           root->level >= root->left->level);
  /* No horizontal left links */
  eassert (root->left->level < root->level);
  /* At most one link on same level */
  eassert (root->right->right->level < root->level);

  /* eassert ((root->right == OVERLAY_SENTINEL) || */
  /*          (! overlay_lt (root->right, root))); */
  /* eassert ((root->left == OVERLAY_SENTINEL) || */
  /*          (overlay_lt (root->left, root))); */
  /* Check we don't have ourselves as a child */
  eassert (root->left != root && root->right != root);

  /* Check the max field */
  eassert (root->max >= root->left->max &&
           root->max >= root->right->max &&
           root->max >= root->char_end);
  eassert (root->max == root->left->max ||
           root->max == root->right->max ||
           root->max == root->char_end);

  /* Check subtrees */
  check_valid_aa_tree (root->left, root);
  check_valid_aa_tree (root->right, root);
}
#endif

/* Rebalancing.  See Andersson's paper for a good explanation.

       B(x)        A(x)
       /  \        /  \
     A(x)  \  ->  /   B(x)
    /  \              /  \

 */
inline static void
ot_skew (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  if (t != OVERLAY_SENTINEL && t->left->level == t->level)
    {
      struct Lisp_Overlay *tmp = t;
      t = *tt = t->left;
      /* t = A, tmp = B */
      tmp->left = t->right;
      t->right = tmp;
      t->parent = tmp->parent;
      tmp->parent = t;
      tmp->max = ot_find_max(tmp);
      t->max = ot_find_max(t);
    }
}

/* Rebalancing.  See Andersson's paper for a good explaination.  This
   is a bit more complicated than his code, since we need to maintain
   max field.

   A(x)
    \
     B(x)   ->    B(x+1)
      \          / \
       C(x)    A(x) C(x)

 */
inline static void
ot_split (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  if (t != OVERLAY_SENTINEL && t->level == t->right->right->level )
    {
      struct Lisp_Overlay *tmp = t;
      t = *tt = t->right;
      /* tmp = A, t = B, t->right = C */
      tmp->right = t->left;
      t->left = tmp;
      t->level++;
      eassert (t->level - 1 == tmp->level);
      t->parent = tmp->parent;
      tmp->parent = t;
      tmp->max = ot_find_max(tmp);
      t->max = ot_find_max(t);
    }
}

#include <stdio.h>
/* This is quite useful for debugging.  */
void
print_tree(struct Lisp_Overlay *tree, unsigned level)
{
  eassert (level < 20);
  for (unsigned i = 0; i < level; i++)
    printf("  ");
  if (tree == OVERLAY_SENTINEL)
    {
      printf("nil\n");
      return;
    }
  printf("%li to %li, max=%li, lvl=%u, %p\n", tree->char_start, tree->char_end,
         tree->max, tree->level, tree);
  print_tree(tree->right, level + 2);
  print_tree(tree->left, level + 2);
}

/* Insert NODE in TREE.  On the way up after the insertion, adjust the
   max field of each node if needed.  */
static void
ot_insert_internal (struct Lisp_Overlay **tree,
                    struct Lisp_Overlay *node,
                    struct Lisp_Overlay *parent)
{
  struct Lisp_Overlay *t = *tree;
  if (t == OVERLAY_SENTINEL)
    {
      node->left = node->right = OVERLAY_SENTINEL;
      node->level = 1;
      node->max = node->char_end;
      node->parent = parent;
      *tree = node;
      return;
    }
  else
    {
      if (ot_lt (node, t))
        {
          ot_insert_internal (&t->left, node, t);
        }
      else
        {
          ot_insert_internal (&t->right, node, t);
        }
    }
  ot_skew (tree);
  ot_split (tree);
  t->max = ot_find_max (t);
}

/* Insert NODE into TREE.  */
void
ot_insert (struct Lisp_Overlay **root,
           struct Lisp_Overlay *node)
{
  CHECK_TREE (*root);
  printf("INSERT\n");
  ot_insert_internal(root, node, NULL);
  CHECK_TREE (*root);
}

inline static struct Lisp_Overlay**
addr_of_parent_pointer (struct Lisp_Overlay *t)
{
  struct Lisp_Overlay *parent = t->parent;
  eassert (parent->left == t || parent->right == t);
  if (parent->left == t)
    return &parent->left;
  else
    return &parent->right;
}

/* Find the immediate succecessor of NODE.  Put it in SUCC and its
   parent in SUCC_PARENT.  */
inline static struct Lisp_Overlay*
ot_succ (struct Lisp_Overlay *node)
{
  struct Lisp_Overlay *t = node->right;
  while (t->left != OVERLAY_SENTINEL)
    {
      t = t->left;
    }
  return t;
}

static void
rebalance_after_delete (struct Lisp_Overlay **tree)
{
  struct Lisp_Overlay *t = *tree;
  t->max = ot_find_max(t);
  if (t->left->level < t->level - 1 || t->right->level < t->level - 1)
    {
      t->level--;
      if (t->right->level > t->level)
        t->right->level = t->level;

      /* Andersson leaves it as 'an exercise for the reader' to
         prove that these rebalancing operations are enough.
         Don't you just love when that happens?  */
      ot_skew (tree);
      ot_skew (&(*tree)->right);
      ot_skew (&(*tree)->right->right);
      ot_split (tree);
      ot_split (&(*tree)->right);
    }
}

inline static bool
ot_rchld_p (struct Lisp_Overlay *t)
{
  eassert (t->parent);
  return t->parent->right == t;
}

inline static bool
ot_lchld_p (struct Lisp_Overlay *t)
{
  eassert (t->parent);
  return t->parent->left == t;
}

inline static void
replace_child (struct Lisp_Overlay *old, struct Lisp_Overlay *new,
               struct Lisp_Overlay **root)
{
  eassert (old != OVERLAY_SENTINEL);
  printf("Replacing %p as child of %p with %p as new child\n",
         old, old->parent, new);
  if (old->parent == NULL)
    {
      eassert (*root == old);
      *root = new;
      return;
    }
  if (ot_rchld_p (old))
    {
      printf("%p was right child of %p\n", old, old->parent);
      old->parent->right = new;
    }
  else
    {
      printf("%p was left child of %p\n", old, old->parent);
      old->parent->left = new;
    }
  if (new != OVERLAY_SENTINEL)
    {
      new->parent = old->parent;
    }
}

void
ot_delete (struct Lisp_Overlay **root, struct Lisp_Overlay *t)
{
  /* Some special cases */
  if (t == OVERLAY_SENTINEL)
    return;

  printf("DELETE t = %p\n", t);
  print_tree(*root, 0);
  /* Only node */
  if (t->left == OVERLAY_SENTINEL && t->right == OVERLAY_SENTINEL
      && t->parent == NULL)
    {
      printf("%p was the only node\n", t);
      eassert (*root == t);
      *root = OVERLAY_SENTINEL;
      return;
    }

  /* REPL shall take t place in the tree.  */
  struct Lisp_Overlay *repl;
  struct Lisp_Overlay *rebalancing_start;
  if (t->left == OVERLAY_SENTINEL && t->right == OVERLAY_SENTINEL)
    {
      repl = OVERLAY_SENTINEL;
      rebalancing_start = t->parent;
      goto dostuff;
    }
  else
    {
      repl = ot_succ (t);
      rebalancing_start = repl->parent == t ? repl : repl->parent;
    }
  printf("repl = %p, we start rebalancing from %p\n", repl, rebalancing_start);

  printf("Replacing %p with %p as child\n", repl, repl->right);
  replace_child (repl, repl->right, root);
  repl->level = t->level;
  repl->left = t->left;
  repl->right = t->right;
  repl->max = ot_find_max (repl);
  repl->left->parent = repl->right->parent = repl;
  printf("Replacing %p with %p as child\n", t, repl);

 dostuff:
  replace_child (t, repl, root);
  for (t = rebalancing_start; t->parent; t = t->parent)
    {
      t->max = ot_find_max (t);
      printf("Rebalancing around %p\n", t);
      rebalance_after_delete (addr_of_parent_pointer (t));
      printf("Gonna rebalance around %p next\n", t->parent);
    }
  eassert (t == *root);
  printf("Rebalancing around root\n");
  rebalance_after_delete (root);
  printf("DONE WITH DELETE\n");
  print_tree(*root, 0);
}


/* Add ELM to vector VECP at IDX.  If necessary adjust SIZE.  */
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

static void
add_to_ot_vec (struct ot_vec *vec, struct Lisp_Overlay *t)
{
  if (vec->idx == vec->size - 1)
    {
      vec->size += 100;
      vec->overlays = xnrealloc (vec->overlays, vec->size,
                                 sizeof (Lisp_Object));
    }
  Lisp_Object tmp;
  XSETMISC (tmp, t);
  vec->overlays[vec->idx] = tmp;
  vec->idx++;
}

/* Add all overlays in TREE which contain POS to the vector pointed to
   by VEC_PTR adjusting IDX and VEC_SIZE as necessary.  The overlay
   must end *after* POS.  */
void
overlays_at_new (struct Lisp_Overlay *tree, ptrdiff_t pos,
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
      overlays_at_new (tree->right, pos, vec_size, vec_ptr, idx);
    }
  overlays_at_new (tree->left, pos, vec_size, vec_ptr, idx);
}

/* This is exactly like `ot_at', except this also includes
   overlays whose end is == POS.  */
void
overlays_around_new (struct Lisp_Overlay *tree, ptrdiff_t pos,
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
      overlays_around_new (tree->right, pos, vec_size, vec_ptr, idx);
    }
  overlays_around_new (tree->left, pos, vec_size, vec_ptr, idx);
}

/* Add all overlays in TREE which have window property w and one
   endpoint at POS to VEC_PTR.  */
void
overlays_ending_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                          struct window *w, ptrdiff_t *vec_size,
                          Lisp_Object **vec_ptr, ptrdiff_t *idx)
{
  Lisp_Object win;
  CHECK_TREE (tree);
  if (tree == OVERLAY_SENTINEL || tree->max < pos)
    return;

  if (tree->char_start == pos || tree->char_end == pos)
    {
      Lisp_Object overlay;
      XSETMISC (overlay, tree);
      win = Foverlay_get (overlay, Qwindow);
      if (! WINDOWP (win) || XWINDOW (win) == w)
        {
          add_to_vec (tree, vec_ptr, vec_size, idx);
        }
    }

  if (tree->char_start <= pos)
    overlays_ending_at (tree->right, pos, w,
                              vec_size, vec_ptr, idx);
  overlays_ending_at (tree->left, pos, w,
                            vec_size, vec_ptr, idx);
}

/* Gather all the overlays at POS which should be evaporated.  That
   is: all the zero size overlays at POS which have the 'evaporate
   property set to non nil.  */
void
ot_evap (struct Lisp_Overlay *tree, ptrdiff_t pos,
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
      ot_evap (tree->right, pos, vec_size, vec_ptr, idx);
    }
  ot_evap (tree->left, pos, vec_size, vec_ptr, idx);
}

/* Gather all overlays which are in BEG to END.  Zero size overlays
   count if they either start at BEG or start at END and END ==
   BUF_END, that is: END is the last position in the buffer.  */
void
overlays_in_new (struct Lisp_Overlay *tree, ptrdiff_t beg,
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
    overlays_in_new (tree->right, beg, end, buf_end,
                     vec_size, vec_ptr, idx);
  overlays_in_new (tree->left, beg, end, buf_end,
                   vec_size, vec_ptr, idx);
}


void
next_overlay_change_new (struct Lisp_Overlay *tree,
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
    next_overlay_change_new (tree->right, pos, best);

  if (tree->char_end > pos)
    {
      enddiff = tree->char_end - pos;
      if (enddiff < *best)
        *best = enddiff;
    }
  next_overlay_change_new (tree->left, pos, best);
}

struct Lisp_Overlay *
ot_next_start (struct Lisp_Overlay *tree, ptrdiff_t pos)
{
  if (tree == OVERLAY_SENTINEL)
    return tree;

  if (tree->char_start <= pos)
    {
      if (tree->right->char_start > pos)
        return tree->right;
      else
        return ot_next_start (tree->right, pos);
    }
  else
    return ot_next_start (tree->left, pos);
}

void
ot_prev_change (struct Lisp_Overlay *tree,
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
  ot_prev_change (tree->left, pos, best);

  if (tree->char_start < pos)
    {
      startdiff = pos - tree->char_start;
      if (startdiff < *best)
        *best = startdiff;
      ot_prev_change (tree->right, pos, best);
    }
}

static void
ot_drop_all_internal (struct buffer *buf,
                                struct Lisp_Overlay *tree)
{
  if (tree == OVERLAY_SENTINEL)
    return;
  ot_drop_all_internal (buf, tree->left);
  ot_drop_all_internal (buf, tree->right);
  modify_overlay (buf, tree->char_start, tree->char_end);
}

/* Drop all overlays in BUF.  */
void
ot_drop_all(struct buffer *buf)
{
  ot_drop_all_internal (buf, buf->overlays_root);
  buf->overlays_root = OVERLAY_SENTINEL;
}

/* Add all nodes in TREE to VEC_PTR, which has size VEC_SIZE, starting
   from IDX.  The nodes will be added in the order they have in the
   tree.  */
static void
ot_all_internal (struct Lisp_Overlay *tree,
                           ptrdiff_t *vec_size,
                           Lisp_Object **vec_ptr,
                           ptrdiff_t *idx)
{
  if (tree == OVERLAY_SENTINEL)
    return;

  ot_all_internal (tree->left, vec_size,
                             vec_ptr, idx);
  add_to_vec (tree, vec_ptr, vec_size, idx);
  ot_all_internal (tree->right, vec_size,
                             vec_ptr, idx);
}

/* Put all nodes from TREE into VEC_PTR, adjusting VEC_SIZE as
   necessary.  */
ptrdiff_t
ot_all (struct Lisp_Overlay *tree, ptrdiff_t *vec_size,
                  Lisp_Object **vec_ptr)
{
  ptrdiff_t n = 0;
  ot_all_internal (tree, vec_size, vec_ptr, &n);
  return n;
}

/* Return the buffer OVERLAY is in or nil if OVERLAY has been
   deleted. */
Lisp_Object
buffer_of_overlay (Lisp_Object overlay)
{
  return XOVERLAY (overlay)->buf;
}

/* Adjust CHARPOS for an insert from FROM_CHAR to TO_CHAR.  */
static void
adjust_pos_for_insert (ptrdiff_t *charpos, ptrdiff_t from_char,
                       ptrdiff_t to_char, bool insertion_type,
                       bool before)
{
  if (*charpos > from_char)
    {
      *charpos += to_char - from_char;
    }
  else if (*charpos == from_char && (insertion_type || before))
    {
      *charpos = to_char;
    }
}

static void
ot_starting_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                struct ot_vec *vec)
{


}

static ptrdiff_t
adjust_tree_for_insert (struct Lisp_Overlay *tree, ptrdiff_t from_char,
                        ptrdiff_t to_char, bool before,
                        Lisp_Object **neg_size, ptrdiff_t *len,
                        ptrdiff_t *idx)
{
  /* If we are at a leaf or all nodes in TREE are before the insert,
   return.  */
}


/* Adjust all nodes in TREE for an insert from FROM_CHAR to TO_CHAR.
   If any overlays end up with a negative size delete the from the
   TREE and reinsert them.  */
void
ot_adjust_for_insert (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char,
                                bool before)
{
  if (tree == OVERLAY_SENTINEL || tree->max < from_char)
    return;

  ot_adjust_for_insert (tree->left, from_char, to_char, before);
  ot_adjust_for_insert (tree->right, from_char, to_char, before);

  /* Adjust the start postions.  */
  adjust_pos_for_insert(&tree->char_start, from_char, to_char,
                        tree->start_insertion_type, before);
  /* Adjust the end postions.  */
  adjust_pos_for_insert(&tree->char_end, from_char, to_char,
                        tree->end_insertion_type, before);

  if (tree->char_end < tree->char_start)
    tree->char_end  = tree->char_start;

  tree->max = ot_find_max (tree);
}

/* Adjust CHARPOS for a delete from FROM_CHAR to TO_CHAR.  */
static void
adjust_pos_for_delete (ptrdiff_t *charpos, ptrdiff_t from_char,
                       ptrdiff_t to_char)
{
  if (*charpos > to_char)
    *charpos -= to_char - from_char;
  else if (*charpos > from_char)
    *charpos = from_char;
}

/* Adjust TREE for a delete from FROM_CHAR to TO_CHAR.  */
void
ot_adjust_for_delete (struct Lisp_Overlay *t,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char)
{
  if (t == OVERLAY_SENTINEL || t->max < from_char)
    return;

  ot_adjust_for_delete(t->left, from_char, to_char);
  ot_adjust_for_delete(t->right, from_char, to_char);

  adjust_pos_for_delete(&t->char_start, from_char, to_char);
  adjust_pos_for_delete(&t->char_end, from_char, to_char);
  eassert (t->char_start <= t->char_end);

  t->max = ot_find_max (t);
}


/* Adjust CHARPOS and BYTEPOS for a replace from FROM_CHAR to TO_CHAR.
 */
static void
adjust_pos_for_replace (ptrdiff_t *charpos, ptrdiff_t from_char,
                        ptrdiff_t old_chars, ptrdiff_t new_chars)
{
  ptrdiff_t diff_chars = new_chars - old_chars;

  if (*charpos >= (from_char + old_chars))
    *charpos += diff_chars;
  else if (*charpos > from_char)
    *charpos = from_char;
}

/* Adjust TREE for a replace from FROM_CHAR, replacing OLD_CHARS
   charecters with NEW_CHARS characters.  */
void
ot_adjust_for_replace (struct Lisp_Overlay *t,
                                 ptrdiff_t from_char,
                                 ptrdiff_t old_chars,
                                 ptrdiff_t new_chars)
{
  if (t == OVERLAY_SENTINEL || t->max <= from_char)
    return;


  ot_adjust_for_replace(t->left, from_char,
                                  old_chars, new_chars);
  ot_adjust_for_replace(t->right, from_char,
                                  old_chars, new_chars);

  adjust_pos_for_replace(&t->char_start, from_char,
                         old_chars, new_chars);
  adjust_pos_for_replace(&t->char_end, from_char,
                         old_chars, new_chars);
  eassert (t->char_start <= t->char_end);

  t->max = ot_find_max(t);

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

  ptrdiff_t noverlays = ot_all(b->overlays_root,
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

DEFUN("overlay-tree", Fot, Sot, 0, 1, 0,
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
  /* defsubr (&Sot_at); */
  defsubr (&Soverlay_right);
  defsubr (&Soverlay_left);
  defsubr (&Soverlays_root);
  defsubr (&Sot);
  defsubr (&Soverlay_subtree);
}
