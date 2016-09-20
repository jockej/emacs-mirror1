
#ifndef OVERLAYS_H
#define OVERLAYS_H

#include <config.h>
#include "lisp.h"

extern struct Lisp_Overlay OVERLAY_SENTINEL_NODE;

extern struct Lisp_Overlay * OVERLAY_SENTINEL;

struct overlay_entry
{
  Lisp_Object overlay;
  Lisp_Object string;
  EMACS_INT priority;
  bool after_string_p;
};

void
overlay_tree_insert (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node);

void
overlay_tree_delete (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node);

void
overlay_tree_drop_all (struct buffer *buf);

Lisp_Object
buffer_of_overlay (Lisp_Object overlay);

ptrdiff_t
overlay_tree_all (struct Lisp_Overlay *tree, ptrdiff_t *vec_size,
                  Lisp_Object **vec_ptr);

ptrdiff_t
overlay_tree_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                 ptrdiff_t *next_ptr, ptrdiff_t *prev_ptr,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 bool chane_req);

void
overlay_tree_zero_size_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                           Lisp_Object hit_list);

ptrdiff_t
overlay_tree_adjust_for_insert (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char,
                                ptrdiff_t from_byte,
                                ptrdiff_t to_byte,
                                bool before);

ptrdiff_t
overlay_tree_adjust_for_delete (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char, ptrdiff_t from_byte,
                                ptrdiff_t to_char, ptrdiff_t to_byte);

ptrdiff_t
overlay_tree_adjust_for_replace (struct Lisp_Overlay *tree,
                                 ptrdiff_t from_char,
                                 ptrdiff_t from_byte,
                                 ptrdiff_t old_chars,
                                 ptrdiff_t old_bytes,
                                 ptrdiff_t new_chars,
                                 ptrdiff_t new_bytes);

ptrdiff_t
overlay_tree_at (struct Lisp_Overlay *tree, Lisp_Object **vecp,
                 ptrdiff_t pos);


#endif /* OVERLAYS_H */
