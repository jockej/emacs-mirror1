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

#include "overlays.h"

const struct Lisp_Overlay sentinel;

const struct Lisp_Overlay *overlay_sentinel;

static struct Lisp_Overlay *overlay_split(struct Lisp_Overlay * root)
{
  struct Lisp_Overlay *right = XOVERLAY (root)->right;
  struct Lisp_Overlay *rightright = XOVERLAY (right)->right;
  if (XOVERLAY (root)->level != 0 &&
      XOVERLAY (rightright)->level == XOVERLAY (root)->level)
    {
      struct Lisp_Overlay *tmp = root;
      root = right;
      XOVERLAY (tmp)->right = XOVERLAY (root)->left;
      XOVERLAY (root)->left = tmp;
      ++XOVERLAY (root)->level;
      XOVERLAY (root)->left = overlay_split (XOVERLAY (root)->right);
    }

  return root;
}

static struct Lisp_Overlay *overlay_skew(struct Lisp_Overlay * root)
{
  /* If we've reach the leaves */
  if (XOVERLAY (root)->level == 0)
    return root;

  struct Lisp_Overlay *left = XOVERLAY (root)->left;
  /* If we have a left horizontal link */
  if (XOVERLAY (left)->level == XOVERLAY (root)->level)
    {
      struct Lisp_Overlay *tmp = root;
      root = left;
      XOVERLAY (tmp)->left = XOVERLAY (root)->right;
      XOVERLAY (root)->right = tmp;
    }
  XOVERLAY (root)->right = overlay_skew (XOVERLAY (root)->right);
  return root;
}
