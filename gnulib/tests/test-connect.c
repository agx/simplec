/* -*- buffer-read-only: t -*- vi: set ro: */
/* DO NOT EDIT! GENERATED AUTOMATICALLY! */
/* Test connecting a client socket.
   Copyright (C) 2011 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#include <sys/socket.h>

#include "signature.h"
SIGNATURE_CHECK (connect, int, (int, const struct sockaddr *, socklen_t));

#include <errno.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "sockets.h"
#include "macros.h"

int
main (void)
{
  gl_sockets_startup (SOCKETS_1_1);

  /* Test behaviour for invalid file descriptors.  */
  {
    struct sockaddr_in addr;

    addr.sin_family = AF_INET;
    inet_pton (AF_INET, "127.0.0.1", &addr.sin_addr);
    addr.sin_port = htons (80);
    {
      errno = 0;
      ASSERT (connect (-1, (const struct sockaddr *) &addr, sizeof (addr))
              == -1);
      ASSERT (errno == EBADF);
    }
    {
      errno = 0;
      ASSERT (connect (99, (const struct sockaddr *) &addr, sizeof (addr))
              == -1);
      ASSERT (errno == EBADF);
    }
  }

  return 0;
}