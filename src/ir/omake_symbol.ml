(*
 * Symbols used everywhere.
 * Eventually, we should collect all the global symbols and
 * put them here.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, California Institute of Technology, and
 * HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified By: Aleksey Nogin @email{anogin@hrl.com}
 * @end[license]
 *)
open Om_symbol

let braces_sym                 = Om_symbol.add "{}"

let builtin_sym                = Om_symbol.add "$builtin"
let map_sym                    = Om_symbol.add "$map"
let pervasives_sym             = Om_symbol.add "Pervasives"
let object_sym                 = Om_symbol.add "Object"
let int_object_sym             = Om_symbol.add "Int"
let float_object_sym           = Om_symbol.add "Float"
let string_object_sym          = Om_symbol.add "String"
let sequence_object_sym        = Om_symbol.add "Sequence"
let array_object_sym           = Om_symbol.add "Array"
let fun_object_sym             = Om_symbol.add "Fun"
let rule_object_sym            = Om_symbol.add "Rule"
let file_object_sym            = Om_symbol.add "File"
let dir_object_sym             = Om_symbol.add "Dir"
let body_object_sym            = Om_symbol.add "Body"
let in_channel_object_sym      = Om_symbol.add "InChannel"
let out_channel_object_sym     = Om_symbol.add "OutChannel"
let in_out_channel_object_sym  = Om_symbol.add "InOutChannel"
let map_object_sym             = Om_symbol.add "Map"
let shell_object_sym           = Om_symbol.add "Shell"
let select_object_sym          = Om_symbol.add "Select"
let pipe_object_sym            = Om_symbol.add "Pipe"
let stat_object_sym            = Om_symbol.add "Stat"
let passwd_object_sym          = Om_symbol.add "Passwd"
let group_object_sym           = Om_symbol.add "Group"
let lexer_object_sym           = Om_symbol.add "Lexer"
let parser_object_sym          = Om_symbol.add "Parser"
let location_object_sym        = Om_symbol.add "Location"
let target_object_sym          = Om_symbol.add "Target"
let options_object_sym         = Om_symbol.add "Options"
let var_object_sym             = Om_symbol.add "Var"
let tm_object_sym              = Om_symbol.add "Tm"

let wild_sym                   = Om_symbol.add "%"
let explicit_target_sym        = Om_symbol.add "$EXPLICIT-TARGET"

let current_prec_sym           = Om_symbol.add "current-prec"

let lex_sym                    = Om_symbol.add "lex"
let name_sym                   = Om_symbol.add "name"
let value_sym                  = Om_symbol.add "value"
let lexer_sym                  = Om_symbol.add "lexer"
let val_sym                    = Om_symbol.add "val"

let read_sym                   = Om_symbol.add "read"
let write_sym                  = Om_symbol.add "write"
let error_sym                  = Om_symbol.add "error"

let st_dev_sym                 = Om_symbol.add "st_dev"
let st_ino_sym                 = Om_symbol.add "st_ino"
let st_kind_sym                = Om_symbol.add "st_kind"
let st_perm_sym                = Om_symbol.add "st_perm"
let st_nlink_sym               = Om_symbol.add "st_nlink"
let st_uid_sym                 = Om_symbol.add "st_uid"
let st_gid_sym                 = Om_symbol.add "st_gid"
let st_rdev_sym                = Om_symbol.add "st_rdev"
let st_size_sym                = Om_symbol.add "st_size"
let st_atime_sym               = Om_symbol.add "st_atime"
let st_mtime_sym               = Om_symbol.add "st_mtime"
let st_ctime_sym               = Om_symbol.add "st_ctime"

let pw_name_sym                = Om_symbol.add "pw_name"
let pw_passwd_sym              = Om_symbol.add "pw_passwd"
let pw_uid_sym                 = Om_symbol.add "pw_uid"
let pw_gid_sym                 = Om_symbol.add "pw_gid"
let pw_gecos_sym               = Om_symbol.add "pw_gecos"
let pw_dir_sym                 = Om_symbol.add "pw_dir"
let pw_shell_sym               = Om_symbol.add "pw_shell"

let gr_name_sym                = Om_symbol.add "gr_name"
let gr_passwd_sym              = Om_symbol.add "gr_passwd"
let gr_gid_sym                 = Om_symbol.add "gr_gid"
let gr_mem_sym                 = Om_symbol.add "gr_mem"

let tm_sec_sym                 = Om_symbol.add "tm_sec"
let tm_min_sym                 = Om_symbol.add "tm_min"
let tm_hour_sym                = Om_symbol.add "tm_hour"
let tm_mday_sym                = Om_symbol.add "tm_mday"
let tm_mon_sym                 = Om_symbol.add "tm_mon"
let tm_year_sym                = Om_symbol.add "tm_year"
let tm_wday_sym                = Om_symbol.add "tm_wday"
let tm_yday_sym                = Om_symbol.add "tm_yday"
let tm_isdst_sym               = Om_symbol.add "tm_isdst"
let tm_time_sym                = Om_symbol.add "tm_time"

let target_sym                 = Om_symbol.add "target"
let target_effects_sym         = Om_symbol.add "effects"
let scanner_deps_sym           = Om_symbol.add "scanner-deps"
let static_deps_sym            = Om_symbol.add "static-deps"
let build_deps_sym             = Om_symbol.add "build-deps"
let build_values_sym           = Om_symbol.add "build-values"
let build_commands_sym         = Om_symbol.add "build-commands"
let output_file_sym            = Om_symbol.add "output-file"

let argv_sym                   = Om_symbol.add "argv"
let star_sym                   = Om_symbol.add "*"
let at_sym                     = Om_symbol.add "@"
let amp_sym                    = Om_symbol.add "&"
let lt_sym                     = Om_symbol.add "<"
let gt_sym                     = Om_symbol.add ">"
let plus_sym                   = Om_symbol.add "+"
let hat_sym                    = Om_symbol.add "^"
let zero_sym                   = Om_symbol.add "0"

let runtime_exception_sym      = Om_symbol.add "RuntimeException"
let unbuildable_exception_sym  = Om_symbol.add "UnbuildableException"
let parse_loc_sym              = Om_symbol.add "parse-loc"
let loc_sym                    = Om_symbol.add "loc"
let pos_sym                    = Om_symbol.add "position"
let message_sym                = Om_symbol.add "message"

let stdin_sym                  = Om_symbol.add "stdin"
let stdout_sym                 = Om_symbol.add "stdout"
let stderr_sym                 = Om_symbol.add "stderr"
let printexitvalue_sym         = Om_symbol.add "printexitvalue"

let targets_sym                = Om_symbol.add "TARGETS"

let glob_options_sym           = Om_symbol.add "GLOB_OPTIONS"
let glob_allow_sym             = Om_symbol.add "GLOB_ALLOW"
let glob_ignore_sym            = Om_symbol.add "GLOB_IGNORE"

let this_sym                   = Om_symbol.add "this"
let dynamic_sym                = Om_symbol.add "dynamic"
let static_sym                 = Om_symbol.add "static"

let allow_empty_subdirs_sym    = Om_symbol.add "ALLOW_EMPTY_SUBDIRS"

let abort_on_command_error_sym     = Om_symbol.add "ABORT_ON_COMMAND_ERROR"
let exit_on_uncaught_exception_sym = Om_symbol.add "EXIT_ON_UNCAUGHT_EXCEPTION"
let create_subdirs_sym             = Om_symbol.add "CREATE_SUBDIRS"

let scanner_mode_sym               = Om_symbol.add "SCANNER_MODE"

let history_file_sym               = Om_symbol.add "history-file"
let history_length_sym             = Om_symbol.add "history-length"

let build_summary_sym              = Om_symbol.add "BUILD_SUMMARY"

(*
 * Special symbols.
 *)
let concat_sym    = Om_symbol.add "concat"

let if_sym        = Om_symbol.add "if"
let else_sym      = Om_symbol.add "else"
let elseif_sym    = Om_symbol.add "elseif"
let switch_sym    = Om_symbol.add "switch"
let select_sym    = Om_symbol.add "select"
let case_sym      = Om_symbol.add "case"
let do_sym        = Om_symbol.add "do"
let while_sym     = Om_symbol.add "while"
let default_sym   = Om_symbol.add "default"
let include_sym   = Om_symbol.add "include"
let section_sym   = Om_symbol.add "section"
let try_sym       = Om_symbol.add "try"
let catch_sym     = Om_symbol.add "catch"
let when_sym      = Om_symbol.add "when"
let finally_sym   = Om_symbol.add "finally"
let curry_sym     = Om_symbol.add "curry"
let private_sym   = Om_symbol.add "private"
let protected_sym = Om_symbol.add "protected"
let public_sym    = Om_symbol.add "public"
let global_sym    = Om_symbol.add "global"
let const_sym     = Om_symbol.add "const"
let rule_sym      = Om_symbol.add "rule"
let system_sym    = Om_symbol.add "system"
let open_sym      = Om_symbol.add "open"
let autoload_sym  = Om_symbol.add "autoload"
let declare_sym   = Om_symbol.add "declare"
let return_sym    = Om_symbol.add "return"
let export_sym    = Om_symbol.add "export"
let value_sym     = Om_symbol.add "value"
let file_sym      = Om_symbol.add "__FILE__"
let file_id_sym   = Om_symbol.add "__ID__"
let foreach_sym   = Om_symbol.add "foreach"
let fun_sym       = Om_symbol.add "fun"
let set_sym       = Om_symbol.add "set"

let neg_fun_sym       = Om_symbol.add "neg"
let add_fun_sym       = Om_symbol.add "add"
let sub_fun_sym       = Om_symbol.add "sub"
let mul_fun_sym       = Om_symbol.add "mul"
let div_fun_sym       = Om_symbol.add "div"
let mod_fun_sym       = Om_symbol.add "mod"
let lsl_fun_sym       = Om_symbol.add "lsl"
let lsr_fun_sym       = Om_symbol.add "lsr"
let asr_fun_sym       = Om_symbol.add "asr"
let lxor_fun_sym      = Om_symbol.add "lxor"
let lor_fun_sym       = Om_symbol.add "lor"
let land_fun_sym      = Om_symbol.add "land"
let and_fun_sym       = Om_symbol.add "and"
let or_fun_sym        = Om_symbol.add "or"
let le_fun_sym        = Om_symbol.add "le"
let lt_fun_sym        = Om_symbol.add "lt"
let equal_fun_sym     = Om_symbol.add "equal"
let nequal_fun_sym    = Om_symbol.add "nequal"
let ge_fun_sym        = Om_symbol.add "ge"
let gt_fun_sym        = Om_symbol.add "gt"
let nth_fun_sym       = Om_symbol.add "nth"

let memo_rule_sym       = Om_symbol.add "memo-rule"

let empty_map_sym       = Om_symbol.add "empty-map"
let create_map_sym      = Om_symbol.add "create-map"
let create_lazy_map_sym = Om_symbol.add "create-lazy-map"

(*
 * Awk values.
 *)
let awk_sym       = Om_symbol.add "awk"
let nf_sym        = Om_symbol.add "NF"
let rs_sym        = Om_symbol.add "RS"
let fs_sym        = Om_symbol.add "FS"

let filename_sym  = Om_symbol.add "FILENAME"
let fnr_sym       = Om_symbol.add "FNR"

let fsubst_sym    = Om_symbol.add "fsubst"

(*
 * The applications that can have cases.
 *)
let cases_syms    = [awk_sym; fsubst_sym]
let cases_set     = List.fold_left SymbolSet.add SymbolSet.empty cases_syms

let clauses_syms  = [case_sym; default_sym; when_sym; catch_sym; finally_sym; do_sym]
let clauses_set   = List.fold_left SymbolSet.add SymbolSet.empty clauses_syms

(*
 * Colon symbols.
 *)
let normal_sym   = Om_symbol.add ":normal:"
let optional_sym = Om_symbol.add ":optional:"
let exists_sym   = Om_symbol.add ":exists:"
let squash_sym   = Om_symbol.add ":squash:"
let effects_sym  = Om_symbol.add ":effects:"
let scanner_sym  = Om_symbol.add ":scanner:"
let values_sym   = Om_symbol.add ":value:"
let key_sym      = Om_symbol.add ":key:"

(*
 * Builtin functions.
 *)
let extends_sym    = Om_symbol.add "extends"
let omakeflags_sym = Om_symbol.add "OMakeFlags"
let omakeargv_sym  = Om_symbol.add "OMakeArgv"

(*
 * Symbols.
 *)
let prompt_sym      = Om_symbol.add "prompt"
let ignoreeof_sym   = Om_symbol.add "ignoreeof"

let cwd_sym         = Om_symbol.add "CWD"
let stdroot_sym     = Om_symbol.add "STDROOT"
let stdlib_sym      = Om_symbol.add "STDLIB"
let ostype_sym      = Om_symbol.add "OSTYPE"
let path_sym        = Om_symbol.add "PATH"
let auto_rehash_sym = Om_symbol.add "AUTO_REHASH"

let omakepath_sym = Om_symbol.add "OMAKEPATH"

let oshell_sym    = Om_symbol.add "OSHELL"

let cdpath_sym    = Om_symbol.add "cdpath"

(*
 * -*-
 * Local Variables:
 * End:
 * -*-
 *)
