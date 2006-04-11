(*
 * Options for the omake program.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Jason Hickey, Caltech
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 *)

(*
 * When to print output.
 *)
type eval_flag =
   EvalNever
 | EvalLazy
 | EvalEager

(*
 * The basic make flags.
 *)
type omake_options =
   { opt_job_count           : int;
     opt_remote_servers      : (string * int) list;
     opt_terminate_on_error  : bool;
     opt_dry_run             : bool;
     opt_print_command       : eval_flag;
     opt_print_dir           : bool;
     opt_print_file          : bool;
     opt_print_status        : bool;
     opt_print_exit          : bool;
     opt_print_progress      : bool;
     opt_touch_only          : bool;
     opt_flush_cache         : bool;
     opt_flush_env           : bool;
     opt_flush_dependencies  : bool;
     opt_print_dependencies  : bool;
     opt_show_dependencies   : string list;
     opt_cd_root             : bool;
     opt_project             : bool;
     opt_poll                : bool;
     opt_poll_on_done        : bool;
     opt_flush_include       : bool;
     opt_flush_static        : bool;
     opt_allow_exceptions    : bool;
   }

(*!
 * @docoff
 *
 * -*-
 * Local Variables:
 * Caml-master: "compile"
 * End:
 * -*-
 *)
