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

static void
overlay_split(struct Lisp_Overlay * root)
{
  if (root->level != 0 &&
      right->right->level == root->level)
    {
      struct Lisp_Overlay *tmp = root;
      root = root->right;
      tmp->right = root->left;
      root->left = tmp;
      ++root->level;
    }

  return root;
}

static void
overlay_skew(struct Lisp_Overlay * root)
{
  /* If we've reach the leaves */
  if (root->level == 0)
    return root;

  struct Lisp_Overlay *left = root->left;
  /* If we have a left horizontal link */
  if (left->level == root->level)
    {
      struct Lisp_Overlay *tmp = root;
      root = left;
      tmp->left = root->right;
      root->right = tmp;
    }
  return root;
}

struct Lisp_Overlay *
overlay_insert(struct Lisp_Overlay *root, struct Lisp_Overlay *elm)
{
  if (root == overlay_sentinel)
    {
      root = elm;
      elm->level = 1;
    }
  else
    {
      if (elm->char_start < root->char_start)
        root->left = overlay_insert(root->left, elm);
      else
        root->right = overlay_insert(root->right, elm);
      root = overlay_skew(root);
      root = overlay_split(root);
    }
  return root;
}

struct Lisp_Overlay *
overlay_delete(struct Lisp_Overlay *root, struct Lisp_Overlay *elm)
{
  static struct Lisp_Overlay *deleted, *last;

  /* Search all the way down the tree */
  if (root != overlay_sentinel)
    {
      replacer = root;
      /* if our interval start point is smaller than root->char_start,
         turn left.  */
      if (elm->char_start < root->char_start)
        {
          root->left = overlay_delete(root->left, elm);
        }
      /* Else turn right and record that this might be the node to
         be deleted  */
      else
        {
          deleted = root;
          root->right = overlay_delete(root->right, elm);
        }
    }

  if (root == last && deleted != sentinel && root == elm)
    {
      deleted->char_start = root->char_start;
      deleted->char_end = root->char_end;
      root = root->right;
      /* Last should be GCed now */

    }
  else if (root->left->level < root->level - 1 ||
          root->right->level < root->level - 1)
    {
      --root->level;
      if (root->right->level > root->level)
        {
          root->right->level = root->level;
        }
      overlay_skew(root);
      overlay_skew(root->right);
      overlay_skew(root->right->right);
      overlay_split(root);
      overlay_split(root->right);
    }
}

  return root;
}
