#ifndef OVERLAYS_H
#define OVERLAYS_H

#include <config.h>
#include "lisp.h"

#ifdef CHECK_OVERLAY_TREE
#define CHECK_TREE(TREE)  check_valid_aa_tree (TREE)
void
check_valid_aa_tree (struct Lisp_Overlay *root);
#else
#define CHECK_TREE(TREE)
#endif

extern struct Lisp_Overlay OVERLAY_SENTINEL_NODE;

extern struct Lisp_Overlay * OVERLAY_SENTINEL;

void
overlay_tree_insert (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node);

void
overlay_tree_delete (struct Lisp_Overlay **tree,
                     struct Lisp_Overlay *node,
                     struct Lisp_Overlay *parent);

void
overlay_tree_drop_all (struct buffer *buf);

Lisp_Object
buffer_of_overlay (Lisp_Object overlay);

ptrdiff_t
overlay_tree_all (struct Lisp_Overlay *tree, ptrdiff_t *vec_size,
                  Lisp_Object **vec_ptr);

void
overlay_tree_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 ptrdiff_t *idx);

void
overlay_tree_around (struct Lisp_Overlay *tree, ptrdiff_t pos,
                     ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                     ptrdiff_t *idx);

void
overlay_tree_endpoint_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                          struct window *w, ptrdiff_t *vec_size,
                          Lisp_Object **vec_ptr, ptrdiff_t *idx);

void
overlay_tree_evap (struct Lisp_Overlay *tree, ptrdiff_t pos,
                   ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                   ptrdiff_t *idx);

void
overlay_tree_in (struct Lisp_Overlay *tree, ptrdiff_t beg,
                 ptrdiff_t end, ptrdiff_t buf_end,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 ptrdiff_t *idx);

void
overlay_tree_next_change(struct Lisp_Overlay *tree,
                         ptrdiff_t pos, ptrdiff_t *best);

struct Lisp_Overlay *
overlay_tree_next_start (struct Lisp_Overlay *tree, ptrdiff_t pos);

void
print_tree(struct Lisp_Overlay *tree, unsigned level);
#define PRINT_TREE(TREE)                        \
  do {                                          \
    print_tree(TREE, 0);                        \
    printf("\n");                               \
  } while (0)

void
overlay_tree_prev_change(struct Lisp_Overlay *tree,
                         ptrdiff_t pos, ptrdiff_t *best);

void
overlay_tree_adjust_for_insert (struct Lisp_Overlay **tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char,
                                bool before);

ptrdiff_t
overlay_tree_adjust_for_delete (struct Lisp_Overlay **tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char);

ptrdiff_t
overlay_tree_adjust_for_replace (struct Lisp_Overlay *tree,
                                 ptrdiff_t from_char,
                                 ptrdiff_t old_chars,
                                 ptrdiff_t new_chars);

void
overlay_tree_copy_tree (struct buffer *from, struct buffer *to);

#endif /* OVERLAYS_H */
