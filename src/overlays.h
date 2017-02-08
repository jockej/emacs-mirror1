#ifndef OVERLAYS_H
#define OVERLAYS_H

#include <config.h>
#include "lisp.h"

#ifdef CHECK_OVERLAY_TREE
#define CHECK_TREE(TREE)  check_valid_aa_tree (TREE, NULL)
void
check_valid_aa_tree (struct Lisp_Overlay *root, struct Lisp_Overlay *parent);
#else
#define CHECK_TREE(TREE)
#endif

struct ot_vec {
  size_t size;
  size_t idx;
  Lisp_Object* overlays;
};

#define INIT_OT_VEC(VEC, SIZE) \
  struct ot_vec VEC = { .size = SIZE, .idx = 0};        \
  VEC.overlays = xnmalloc (VEC.size, sizeof (Lisp_Object));

extern struct Lisp_Overlay OVERLAY_SENTINEL_NODE;

extern struct Lisp_Overlay * OVERLAY_SENTINEL;

void
ot_insert (struct Lisp_Overlay **tree, struct Lisp_Overlay *node);

void
ot_delete (struct Lisp_Overlay **tree, struct Lisp_Overlay *t);

void
ot_drop_all (struct buffer *buf);

Lisp_Object
buffer_of_overlay (Lisp_Object overlay);

ptrdiff_t
ot_all (struct Lisp_Overlay *tree, ptrdiff_t *vec_size,
                  Lisp_Object **vec_ptr);

void
overlays_at_new (struct Lisp_Overlay *tree, ptrdiff_t pos,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 ptrdiff_t *idx);

void
overlays_around_new (struct Lisp_Overlay *tree, ptrdiff_t pos,
                     ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                     ptrdiff_t *idx);

void
overlays_ending_at (struct Lisp_Overlay *tree, ptrdiff_t pos,
                          struct window *w, ptrdiff_t *vec_size,
                          Lisp_Object **vec_ptr, ptrdiff_t *idx);

void
ot_evap (struct Lisp_Overlay *tree, ptrdiff_t pos,
                   ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                   ptrdiff_t *idx);

void
overlays_in_new (struct Lisp_Overlay *tree, ptrdiff_t beg,
                 ptrdiff_t end, ptrdiff_t buf_end,
                 ptrdiff_t *vec_size, Lisp_Object **vec_ptr,
                 ptrdiff_t *idx);

void
next_overlay_change_new (struct Lisp_Overlay *tree,
                         ptrdiff_t pos, ptrdiff_t *best);

struct Lisp_Overlay*
ot_next_start (struct Lisp_Overlay *tree, ptrdiff_t pos);

void
print_tree(struct Lisp_Overlay *tree, unsigned level);
#define PRINT_TREE(TREE)                        \
  do {                                          \
    print_tree(TREE, 0);                        \
    printf("\n");                               \
  } while (0)

void
ot_prev_change(struct Lisp_Overlay *tree,
                         ptrdiff_t pos, ptrdiff_t *best);

void
ot_adjust_for_insert (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char,
                                bool before);

void
ot_adjust_for_delete (struct Lisp_Overlay *tree,
                                ptrdiff_t from_char,
                                ptrdiff_t to_char);

void
ot_adjust_for_replace (struct Lisp_Overlay *tree,
                                 ptrdiff_t from_char,
                                 ptrdiff_t old_chars,
                                 ptrdiff_t new_chars);

void
ot_copy_tree (struct buffer *from, struct buffer *to);

#endif /* OVERLAYS_H */
