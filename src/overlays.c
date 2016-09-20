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
   possible to the one presented in Anderssons paper (REF)[1], but with a
   few differences.  The code in [1] is written in Pascal, which has
   proper pass by reference. Unfortunately C doesn't really have this;
   this is the reason for passing around pointers to pointers.

   Also this tree has parent references, and the code in the delete
   routine is a bit more complicated because we need to delete the
   actual memory area, so as to not trip up the GC.

   The fact that this is an augmented tree also makes the rebalancing
   operation (split and skew) a bit more complex.
 */
#include "overlays.h"
#include "buffer.h"

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
    Lisp_Misc_Overlay,          /* type */
    1,                          /* gcmarkbit */
    0,                          /* start_insertion_type */
    0,                          /* end_insertion_type */
    0,                          /* spacer */
    NULL,                       /* next */
    0,                          /* start */
    0,                          /* end */
    0,                          /* plist */
    0,                          /* char_start */
    0,                          /* char_end */
    0,                          /* byte_start */
    0,                          /* byte_end */
    0,                          /* max */
    0,                          /* parent */
    &OVERLAY_SENTINEL_NODE,     /* left */
    &OVERLAY_SENTINEL_NODE,     /* right */
    0                           /* level */
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

  return false;
}

/* Rebalancing.  See Andersson's paper for a good explaination.  This
   is a bit more complicated than his code, since we need to maintain
   parent pointer and max field.
*/
inline static void
overlay_skew (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  if (t->left->level == t->level && t != OVERLAY_SENTINEL)
    {
      struct Lisp_Overlay *tmp = t;
      t = *tt = t->left;
      tmp->left = t->right;
      t->right = tmp;

      tmp->max = overlay_find_max(tmp);
      t->max = overlay_find_max(t);

      t->parent = tmp->parent;
      XSETMISC (tmp->parent, t);
      if (tmp->left != OVERLAY_SENTINEL)
        XSETMISC (tmp->left->parent, tmp);
    }
}

/* Rebalancing.  See Andersson's paper for a good explaination.  This
   is a bit more complicated than his code, since we need to maintain
   parent pointer and max field.
*/
inline static void
overlay_split (struct Lisp_Overlay **tt)
{
  struct Lisp_Overlay *t = *tt;
  if (t->level == t->right->right->level && t != OVERLAY_SENTINEL)
    {
      struct Lisp_Overlay *tmp = t;
      t = *tt = t->right;
      tmp->right = t->left;
      t->left = tmp;
      t->level++;

      tmp->max = overlay_find_max(tmp);
      t->max = overlay_find_max(t);

      t->parent = tmp->parent;
      XSETMISC (tmp->parent, t);
    }
}

/* Insert NODE in TREE.  When it is inserted, set its parent to
   PARENT.  On the way up after the insertion, adjust the max field of
   each node if needed.
 */
static ptrdiff_t
overlay_tree_insert_internal (struct Lisp_Overlay **tree,
                              struct Lisp_Overlay *parent,
                              struct Lisp_Overlay *node)
{
  struct Lisp_Overlay *t = *tree;
  if (t == OVERLAY_SENTINEL)
    {
      node->left = node->right = OVERLAY_SENTINEL;
      node->level = 1;
      node->max = node->char_end;
      XSETMISC (node->parent, parent);
      *tree = node;
      return node->max;
    }
  else
    {
      struct Lisp_Overlay **dir = overlay_lt (node, t) ?
        &t->left : &t->right;

      ptrdiff_t child_max = overlay_tree_insert_internal(dir, t, node);
      if (child_max > t->max)
        t->max = child_max;
    }
  overlay_skew (&t);
  overlay_split (&t);
  return t->max;
}

/* Insert NODE into TREE.
 */
void
overlay_tree_insert (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node)
{
  overlay_tree_insert_internal(tree, *tree, node);
}

/* Delete NODE from TREE.
 */
void
overlay_tree_delete (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node)
{
  struct Lisp_Overlay *t = *tree;
  static __thread struct Lisp_Overlay *last, *deleted;

  if (t == OVERLAY_SENTINEL)
    return;

  last = t;
  if (overlay_lt(node, t))
    overlay_tree_delete (&t->left, node);
  else
    {
      deleted = t;
      overlay_tree_delete (&t->right, node);
    }


  if (t == last &&
      deleted != OVERLAY_SENTINEL &&
      node == deleted)
    {
      last->left = deleted->left;
      last->right = deleted->right;

      if (BUFFERP (deleted->parent))
        {
          struct buffer *b = XBUFFER (deleted->parent);
          if (last == deleted)
            {
              b->overlays_root = OVERLAY_SENTINEL;
            }
          else
            {
              b->overlays_root = last;
              last->parent = deleted->parent;
            }
        }
      else
        {
          eassert (OVERLAYP (deleted->parent));
          struct Lisp_Overlay *up = XOVERLAY (deleted->parent);
          eassert (up->left == deleted || up->right == deleted);
          if (up->left == deleted)
            up->left = last == deleted ? OVERLAY_SENTINEL
              : last;
          else
            up->right = last == deleted ? OVERLAY_SENTINEL
              : last;

          XSETMISC (last->parent, up);
        }
      deleted->parent = Qnil;
    }
  else if (t->left->level < t->level - 1
           || t->right->level < t->level - 1)
    {
      t->level--;
      if (t->right->level > t->level)
        t->right->level = t->level;

      /* Andersson leaves it as 'an exercise for the reader' to prove
      that these rebalancing operions are enough.  Don't you just love
      when that happens?  */
      overlay_skew (&t);
      overlay_skew (&t->right);
      overlay_skew (&t->right->right);
      overlay_split (&t);
      overlay_split (&t->right);
    }
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
overlay_tree_drop_all(struct buffer *buf)
{
  overlay_tree_drop_all_internal (buf, buf->overlays_root);
  buf->overlays_root = OVERLAY_SENTINEL;
}

/* Add ELM to VECP at IDX.  VECP has size VEC_SIZE.  If IDX is at the
   end of VECP, realloc VECP and update VEC_SIZE.
 */
static inline void
add_to_vec (ptrdiff_t *vec_size, Lisp_Object **vecp,
            ptrdiff_t* idx, struct Lisp_Overlay *elm)
{
  if (*idx == *vec_size - 1)
    {
      *vec_size += 50;
      *vecp = xnrealloc (*vecp, *vec_size, sizeof (Lisp_Object));
    }

  XSETMISC((*vecp)[(*idx)++], elm);
}


/* Add all nodes in TREE to VEC_PTR, which has size VEC_SIZE, starting
   from IDX.  The nodes will be added in the order they have in the
   tree.
 */
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
  add_to_vec (vec_size, vec_ptr, idx, tree);
  overlay_tree_all_internal (tree->right, vec_size,
                             vec_ptr, idx);
}

/* Put all nodes from TREE into VEC_PTR, adjusting VEC_SIZE as
   necessary.
 */
ptrdiff_t
overlay_tree_all (struct Lisp_Overlay *tree, ptrdiff_t *vec_size,
                  Lisp_Object **vec_ptr)
{
  ptrdiff_t n = 0;
  overlay_tree_all_internal (tree, vec_size, vec_ptr, &n);
  return n;
}

/* Add all nodes in TREE which contain POS to VEC_PTR at IDX.
   VEC_SIZE will be adjusted.
 */
static void
overlay_tree_at_internal (struct Lisp_Overlay *tree, ptrdiff_t pos,
                          ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                          ptrdiff_t *idx, ptrdiff_t *prev_ptr)
{
  /* We are at a leaf.  */
  if (tree == OVERLAY_SENTINEL)
    return;

  /* There's no subtree under here which can contain POS.  Note
  tree->max, as this might be the closest before.  */
  if (tree->max < pos)
    {
      if (tree->max > *prev_ptr)
        *prev_ptr = tree->max;
      return;
    }


  overlay_tree_at_internal (tree->left, pos, vec_size, vec_ptr,
                            idx, prev_ptr);

  if (pos >= tree->char_start && pos <= tree->char_end)
    add_to_vec (vec_size, vec_ptr, idx, tree);

  /* If we are after POS, so are all the nodes to the right of us.  */
  if (tree->char_start <= pos)
    overlay_tree_at_internal (tree->right, pos, vec_size, vec_ptr,
                              idx, prev_ptr);
}


/* Find all nodes in TREE which contain POS and put them in VEC_PTR,
   growing it as necessary.  The size of the vector VEC_PTR will be
   stored in VEC_SIZE.  Return how many nodes were actually put in
   VEC_PTR.
 */
ptrdiff_t
overlay_tree_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                 ptrdiff_t *next_ptr, ptrdiff_t *prev_ptr,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 bool chane_req)
{
  ptrdiff_t idx = 0;
  ptrdiff_t max_before = 0;

    /* Due to the recursion order in `overlay_tree_at_internal' the
  overlays are sorted by their `char_start' in VEC_PTR.  */
  overlay_tree_at_internal (tree, pos, vec_size, vec_ptr,
                            &idx, &max_before);

  if (prev_ptr && max_before)
    *prev_ptr = max_before;
  else
    *prev_ptr = BEGV;

  /* If NEXT_PTR is not NULL, it should be set to the start_char of
  the leftmost descendant of the right child of the last element in
  VEC_PTR, or ZV if the right child is OVERLAY_SENTINEL.  */
  if (next_ptr && idx)
    {
      struct Lisp_Overlay *last = XOVERLAY ((*vec_ptr)[idx - 1]);
      if (last->right != OVERLAY_SENTINEL)
        {
          last = last->right;
          while (last->left != OVERLAY_SENTINEL)
            last = last->left;
          *next_ptr = last->char_start;
        }
      else
        *next_ptr = ZV;
    }

  /* IDX points one behind the last element, so it is the size */
  return idx;
}

static inline void
add_entry_to_vec (ptrdiff_t *vec_size, struct overlay_entry **vecp,
                  ptrdiff_t* idx, Lisp_Object overlay,
                  Lisp_Object str, bool after_p)
{
  if (*idx == *vec_size - 1)
    {
      *vec_size += 50;
      *vecp = xnrealloc (*vecp, *vec_size,
                         sizeof (struct overlay_entry));
    }

  Lisp_Object priority = Foverlay_get ((OVERLAY), Qpriority);
  EMACS_INT prio = INTEGERP (priority) ? XINT (priority) : 0;

#define SET_ENTRY(ENT, ELM)  (*vecp)[*idx].ENT = (ELM)
  SET_ENTRY(string, str);
  SET_ENTRY(overlay, overlay);
  SET_ENTRY(priority, prio);
  SET_ENTRY(after_string, after_p);
#undef SET_ENTRY

  (*idx)++;
}

ptrdiff_t
overlay_tree_load_overlays (struct Lisp_Overlay *tree, ptrdiff_t pos,
                            ptrdiff_t *vec_size, ptrdiff_t **vec_ptr,
                            ptridff_t *idx, struct window *w)
{
  Lisp_Object window, invisible, str, ov;
  int invis;

  eassert (*idx == 0);

  if (tree == OVERLAY_SENTINEL || tree->max > pos)
    return;

  if (tree->char_start != pos && tree->char_end != pos)
    goto cont;

  window = lookup_char_property (tree->plist, Qwindow, 0);
  if (WINDOWP (window) && XWINDOW (window) != it->w)
    goto cont;

  invisible = lookup_char_property (tree->plist, Qinvisible, 0);
  invis = TEXT_PROP_MEANS_INVISIBLE (invisible);

  if ((tree->char_start == pos || (tree->char_end == pos && invis))
      && (str = lookup_char_property (tree->plist, Qbefore_string, 0),
          STRINGP (str))
      && SCHARS (str))
    {
      XSETMISC (ov, tree);
      add_entry_to_vec(vec_size, vec_ptr, idx, ov, str, false);
    }

  if ((tree->char_end == pos || (tree->char_start == pos && invis))
      && (str = lookup_char_property (tree->plist, Qafter_string, 0),
          STRINGP (str))
      && SCHARS (str))
    {
      XSETMISC (ov, tree);
      add_entry_to_vec(vec_size, vec_ptr, idx, ov, str, true);
    }


 cont:
  overlay_tree_load_overlays(tree->left, pos, vec_size, vec_ptr, w);
  if (tree->char_start <= pos)
    overlay_tree_load_overlays(tree->right, pos, vec_size, vec_ptr, w);
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

/* Adjust CHARPOS asn BYTEPOS for an insert from FROM_CHAR (FROM_BYTE)
   to TO_CHAR (TO_BYTE).
   FIXME: insertion_type and before.
 */
static void
adjust_pos_for_insert (ptrdiff_t *charpos, ptrdiff_t *bytepos,
                       ptrdiff_t from_char, ptrdiff_t to_char,
                       ptrdiff_t from_byte, ptrdiff_t to_byte,
                       bool insertion_type , bool before)
{
  if (*bytepos > from_byte)
    {
      *bytepos += to_byte - from_byte;
      *charpos += to_char - from_char;
    }
  else if (*bytepos == from_byte && insertion_type)
    {
      *bytepos = to_byte;
      *charpos = to_char;
    }
}

/* Adjust all nodes in TREE for an insert from FROM_CHAR (FROM_BYTE)
   to TO_CHAR (TO_BYTE).  Return TREEs max.
   FIXME: before.
 */
ptrdiff_t
overlay_tree_adjust_for_insert (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char,
                                ptrdiff_t from_byte,
                                ptrdiff_t to_byte,
                                bool before)
{
  /* If we are at a leaf or all nodes in TREE are before the insert,
   return.  */
  if (tree == OVERLAY_SENTINEL || tree->max < from_char)
    return tree->max;

  /* Adjust the start postions.  */
  adjust_pos_for_insert(&tree->char_start, &tree->byte_start, from_char,
                        to_char, from_byte, to_byte,
                        tree->start_insertion_type, before);
  /* Adjust the end postions.  */
  adjust_pos_for_insert(&tree->char_end, &tree->byte_end, from_char,
                        to_char, from_byte, to_byte,
                        tree->end_insertion_type, before);

  ptrdiff_t r,l;

  l = overlay_tree_adjust_for_insert (tree->left, from_char,
                                      to_char, from_byte,
                                      to_byte, before);
  r = overlay_tree_adjust_for_insert (tree->right, from_char,
                                      to_char, from_byte,
                                      to_byte, before);

  tree->max = overlay_max(l, r, tree->char_end);
  return tree->max;
}

/* Adjust CHARPOS and BYTEPOS for a delete from FROM_CHAR (FROM_BYTE)
   to TO_CHAR (TO_BYTE).
 */
static void
adjust_pos_for_delete (ptrdiff_t *charpos, ptrdiff_t *bytepos,
                       ptrdiff_t from_char, ptrdiff_t from_byte,
                       ptrdiff_t to_char, ptrdiff_t to_byte)
{
  if (*charpos > to_char)
    {
      *charpos = to_char - from_char;
      *bytepos = to_byte - from_byte;
    }
  else if (*charpos > from_char)
    {
      *charpos = from_char;
      *bytepos = from_byte;
    }
}

/* Adjust TREE for a delete from FROM_CHAR (FROM_BYTE) to TO_CHAR
   (TO_BYTE).
 */
ptrdiff_t
overlay_tree_adjust_for_delete (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char, ptrdiff_t from_byte,
                                ptrdiff_t to_char, ptrdiff_t to_byte)
{
  if (tree == OVERLAY_SENTINEL || tree->max < from_char)
    return tree->max;

  adjust_pos_for_delete(&tree->char_start, &tree->byte_start, from_char,
                        from_byte, to_char, to_byte);
  adjust_pos_for_delete(&tree->char_end, &tree->byte_end, from_char,
                        from_byte, to_char, to_byte);

  ptrdiff_t r, l;

  l = overlay_tree_adjust_for_delete(tree->left, from_char, from_byte,
                                     to_char, to_byte);
  r = overlay_tree_adjust_for_delete(tree->right, from_char, from_byte,
                                     to_char, to_byte);

  tree->max = overlay_max(l, r, tree->char_end);
  return tree->max;
}

/* Adjust CHARPOS and BYTEPOS for a delete from FROM_CHAR (FROM_BYTE)
   to TO_CHAR (TO_BYTE).
 */
static void
adjust_pos_for_replace (ptrdiff_t *charpos, ptrdiff_t *bytepos,
                        ptrdiff_t from_char, ptrdiff_t from_byte,
                        ptrdiff_t old_chars, ptrdiff_t old_bytes,
                        ptrdiff_t new_chars, ptrdiff_t new_bytes)
{
  ptrdiff_t diff_chars = new_chars - old_chars;
  ptrdiff_t diff_bytes = new_bytes - old_bytes;

  if (*bytepos >= (from_byte + old_bytes))
    {
      *charpos += diff_chars;
      *bytepos += diff_bytes;
    }
  else if (*bytepos > from_byte)
    {
      *charpos = from_char;
      *bytepos = from_byte;
    }
}

/* Adjust TREE for a delete from FROM_CHAR (FROM_BYTE)
   to TO_CHAR (TO_BYTE).
 */
ptrdiff_t
overlay_tree_adjust_for_replace (struct Lisp_Overlay *tree,
                                 ptrdiff_t from_char,
                                 ptrdiff_t from_byte,
                                 ptrdiff_t old_chars,
                                 ptrdiff_t old_bytes,
                                 ptrdiff_t new_chars,
                                 ptrdiff_t new_bytes)
{
  if (tree == OVERLAY_SENTINEL || tree->max <= from_byte)
    return tree->max;

  adjust_pos_for_replace(&tree->char_start, &tree->byte_start,
                         from_char, from_byte, old_chars, old_bytes,
                         new_chars, new_bytes);
  adjust_pos_for_replace(&tree->char_end, &tree->byte_end,
                         from_char, from_byte, old_chars, old_bytes,
                         new_chars, new_bytes);

  ptrdiff_t r, l;

  l = overlay_tree_adjust_for_replace(tree->left, from_char,
                                      from_byte, old_chars,
                                      old_bytes, new_chars,
                                      new_bytes);
  r = overlay_tree_adjust_for_replace(tree->right, from_char,
                                      from_byte, old_chars,
                                      old_bytes, new_chars,
                                      new_bytes);

  tree->max = overlay_max(l, r, tree->char_end);
  return tree->max;
}


DEFUN("overlay-parent", Foverlay_parent, Soverlay_parent, 1, 1, 0,
      doc: /* Parent of overlay.  An overlay or a buffer.  */)
  (Lisp_Object overlay)
{
  if (!OVERLAYP(overlay))
    signal_error("Not an overlay", Qnil);
  return XOVERLAY (overlay)->parent;
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


  ptrdiff_t noverlays = overlay_tree_at(b->overlays_root, bufpos,
                                        &next, &prev,
                                        &vec_size, &vec, false);

  ret = Flist(noverlays, vec);

  return ret;
}

void
syms_of_overlays (void)
{
  defsubr (&Soverlay_parent);
  defsubr (&Soverlay_info);
  defsubr (&Soverlays_in_buffer);
  defsubr (&Soverlay_tree_at);
}
