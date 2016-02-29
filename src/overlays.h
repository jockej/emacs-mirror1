#ifndef OVERLAYS_H
#define OVERLAYS_H
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

/* #include "lisp.h"               /\* For the Lisp_Overlay type *\/ */


extern const struct Lisp_Overlay *overlay_sentinel;

struct Lisp_Overlay *overlay_insert(struct Lisp_Overlay *root,
                                    struct Lisp_Overlay *node);

struct Lisp_Overlay *overlay_delete(struct Lisp_Overlay *root,
                                    struct Lisp_Overlay *node);

#endif /* ifndef OVERLAYS_H */
