
QUICK REFERENCE: MAPS				*netrw-browse-maps*
>
	  ---			-----------------			----
	  Map			Quick Explanation			Link
	  ---			-----------------			----
<	 <F1>	Causes Netrw to issue help
	 <cr>	Netrw will enter the directory or read the file      |netrw-cr|
	 <del>	Netrw will attempt to remove the file/directory      |netrw-del|
	 <c-h>	Edit file hiding list                                |netrw-ctrl-h|
	 <c-l>	Causes Netrw to refresh the directory listing        |netrw-ctrl-l|
	 <c-r>	Browse using a gvim server                           |netrw-ctrl-r|
	 <c-tab> Shrink/expand a netrw/explore window                |netrw-c-tab|
	   -	Makes Netrw go up one directory                      |netrw--|
	   a	Toggles between normal display,                      |netrw-a|
	    	hiding (suppress display of files matching g:netrw_list_hide)
	    	showing (display only files which match g:netrw_list_hide)
	   c	Make browsing directory the current directory        |netrw-c|
	   C	Setting the editing window                           |netrw-C|
	   d	Make a directory                                     |netrw-d|
	   D	Attempt to remove the file(s)/directory(ies)         |netrw-D|
	   gb	Go to previous bookmarked directory                  |netrw-gb|
	   gd	Force treatment as directory                         |netrw-gd|
	   gf	Force treatment as file                              |netrw-gf|
	   gh	Quick hide/unhide of dot-files                       |netrw-gh|
	   gn	Make top of tree the directory below the cursor      |netrw-gn|
	   i	Cycle between thin, long, wide, and tree listings    |netrw-i|
	   mb	Bookmark current directory                           |netrw-mb|
	   mc	Copy marked files to marked-file target directory    |netrw-mc|
	   md	Apply diff to marked files (up to 3)                 |netrw-md|
	   me	Place marked files on arg list and edit them         |netrw-me|
	   mf	Mark a file                                          |netrw-mf|
	   mF	Unmark files                                         |netrw-mF|
	   mg	Apply vimgrep to marked files                        |netrw-mg|
	   mh	Toggle marked file suffices' presence on hiding list |netrw-mh|
	   mm	Move marked files to marked-file target directory    |netrw-mm|
	   mp	Print marked files                                   |netrw-mp|
	   mr	Mark files using a shell-style |regexp|                |netrw-mr|
	   mt	Current browsing directory becomes markfile target   |netrw-mt|
	   mT	Apply ctags to marked files                          |netrw-mT|
	   mu	Unmark all marked files                              |netrw-mu|
	   mv	Apply arbitrary vim   command to marked files        |netrw-mv|
	   mx	Apply arbitrary shell command to marked files        |netrw-mx|
	   mX	Apply arbitrary shell command to marked files en bloc|netrw-mX|
	   mz	Compress/decompress marked files                     |netrw-mz|
	   o	Enter the file/directory under the cursor in a new   |netrw-o|
	    	browser window.  A horizontal split is used.
	   O	Obtain a file specified by cursor                    |netrw-O|
	   p	Preview the file                                     |netrw-p|
	   P	Browse in the previously used window                 |netrw-P|
	   qb	List bookmarked directories and history              |netrw-qb|
	   qf	Display information on file                          |netrw-qf|
	   qF	Mark files using a quickfix list                     |netrw-qF|
mycli.log
	   qL	Mark files using a |location-list|                     |netrw-qL|
	   r	Reverse sorting order                                |netrw-r|
	   R	Rename the designated file(s)/directory(ies)         |netrw-R|
	   s	Select sorting style: by name, time, or file size    |netrw-s|
	   S	Specify suffix priority for name-sorting             |netrw-S|
	   t	Enter the file/directory under the cursor in a new tab|netrw-t|
	   u	Change to recently-visited directory                 |netrw-u|
	   U	Change to subsequently-visited directory             |netrw-U|
	   v	Enter the file/directory under the cursor in a new   |netrw-v|
	    	browser window.  A vertical split is used.
	   x	View file with an associated program                 |netrw-x|
	   X	Execute filename under cursor via |system()|           |netrw-X|

	   %	Open a new file in netrw's current directory         |netrw-%|

	*netrw-mouse* *netrw-leftmouse* *netrw-middlemouse* *netrw-rightmouse*
	<leftmouse>	(gvim only) selects word under mouse as if a <cr>
			had been pressed (ie. edit file, change directory)
	<middlemouse>	(gvim only) same as P selecting word under mouse;
			see |netrw-P|
	<rightmouse>	(gvim only) delete file/directory using word under
			mouse
	<2-leftmouse>	(gvim only) when:
			 * in a netrw-selected file, AND
			 * |g:netrw_retmap| == 1       AND
			 * the user doesn't already have a <2-leftmouse>
			   mapping defined before netrw is autoloaded,
			then a double clicked leftmouse button will return
			to the netrw browser window.  See |g:netrw_retmap|.
	<s-leftmouse>	(gvim only) like mf, will mark files.  Dragging
			the shifted leftmouse will mark multiple files.
			(see |netrw-mf|)

	(to disable mouse buttons while browsing: |g:netrw_mousemaps|)

				*netrw-quickcom* *netrw-quickcoms*
QUICK REFERENCE: COMMANDS	*netrw-explore-cmds* *netrw-browse-cmds* {{{2
     :NetrwClean[!]............................................|netrw-clean|
     :NetrwSettings............................................|netrw-settings|
     :Ntree....................................................|netrw-ntree|
     :Explore[!]  [dir] Explore directory of current file......|netrw-explore|
     :Hexplore[!] [dir] Horizontal Split & Explore.............|netrw-explore|
     :Lexplore[!] [dir] Left Explorer Toggle...................|netrw-explore|
     :Nexplore[!] [dir] Vertical Split & Explore...............|netrw-explore|
     :Pexplore[!] [dir] Vertical Split & Explore...............|netrw-explore|
     :Rexplore          Return to Explorer.....................|netrw-explore|
     :Sexplore[!] [dir] Split & Explore directory .............|netrw-explore|
     :Texplore[!] [dir] Tab & Explore..........................|netrw-explore|
     :Vexplore[!] [dir] Vertical Split & Explore...............|netrw-explore|


BANNER DISPLAY						*netrw-I*

One may toggle the banner display on and off by pressing "I".

Also See: |g:netrw_banner|


BOOKMARKING A DIRECTORY *netrw-mb* *netrw-bookmark* *netrw-bookmarks* {{{2

One may easily "bookmark" the currently browsed directory by using >

	mb
<
								*.netrwbook*
Bookmarks are retained in between sessions in a $HOME/.netrwbook file, and are
kept in sorted order.

If there are marked files and/or directories, mb will add them to the bookmark
list.

*netrw-:NetrwMB*
Addtionally, one may use :NetrwMB to bookmark files or directories. >

	:NetrwMB[!] [files/directories]

< No bang: enters files/directories into Netrw's bookmark system

   No argument and in netrw buffer:
     if there are marked files        : bookmark marked files
     otherwise                        : bookmark file/directory under cursor
   No argument and not in netrw buffer: bookmarks current open file
   Has arguments                      : |glob()|s each arg and bookmarks them

 With bang: deletes files/directories from Netrw's bookmark system

The :NetrwMB command is available outside of netrw buffers (once netrw has been
invoked in the session).

The file ".netrwbook" holds bookmarks when netrw (and vim) is not active.  By
default, it's stored on the first directory on the user's |'runtimepath'|.

Related Topics:
	|netrw-gb| how to return (go) to a bookmark
	|netrw-mB| how to delete bookmarks
	|netrw-qb| how to list bookmarks
	|g:netrw_home| controls where .netrwbook is kept


BROWSING					*netrw-enter*	*netrw-cr* {{{2

Browsing is simple: move the cursor onto a file or directory of interest.
Hitting the <cr> (the return key) will select the file or directory.
Directories will themselves be listed, and files will be opened using the
protocol given in the original read request.

  CAVEAT: There are four forms of listing (see |netrw-i|).  Netrw assumes that
  two or more spaces delimit filenames and directory names for the long and
  wide listing formats.  Thus, if your filename or directory name has two or
  more sequential spaces embedded in it, or any trailing spaces, then you'll
  need to use the "thin" format to select it.

The |g:netrw_browse_split| option, which is zero by default, may be used to
cause the opening of files to be done in a new window or tab instead of the
default.  When the option is one or two, the splitting will be taken
horizontally or vertically, respectively.  When the option is set to three, a
<cr> will cause the file to appear in a new tab.


When using the gui (gvim), one may select a file by pressing the <leftmouse>
button.  In addition, if

 * |g:netrw_retmap| == 1       AND   (its default value is 0)
 * in a netrw-selected file, AND
 * the user doesn't already have a <2-leftmouse> mapping defined before
   netrw is loaded

then a doubly-clicked leftmouse button will return to the netrw browser
window.

Netrw attempts to speed up browsing, especially for remote browsing where one
may have to enter passwords, by keeping and re-using previously obtained
directory listing buffers.  The |g:netrw_fastbrowse| variable is used to
control this behavior; one may have slow browsing (no buffer re-use), medium
speed browsing (re-use directory buffer listings only for remote directories),
and fast browsing (re-use directory buffer listings as often as possible).
The price for such re-use is that when changes are made (such as new files
are introduced into a directory), the listing may become out-of-date.  One may
always refresh directory listing buffers by pressing ctrl-L (see
|netrw-ctrl-l|).

								*netrw-s-cr*
Squeezing the Current Tree-Listing Directory~

When the tree listing style is enabled (see |netrw-i|) and one is using
gvim, then the <s-cr> mapping may be used to squeeze (close) the
directory currently containing the cursor.

Otherwise, one may remap a key combination of one's own choice to get
this effect: >

    nmap <buffer> <silent> <nowait> YOURKEYCOMBO  <Plug>NetrwTreeSqueeze
<
Put this line in $HOME/ftplugin/netrw/netrw.vim; it needs to be generated
for netrw buffers only.

Related topics:
	|netrw-ctrl-r|	|netrw-o|	|netrw-p|
	|netrw-P|	|netrw-t|	|netrw-v|
Associated setting variables:
   |g:netrw_browse_split|	|g:netrw_fastbrowse|
   |g:netrw_ftp_list_cmd|	|g:netrw_ftp_sizelist_cmd|
   |g:netrw_ftp_timelist_cmd|	|g:netrw_ssh_browse_reject|
   |g:netrw_ssh_cmd|		|g:netrw_use_noswf|


BROWSING WITH A HORIZONTALLY SPLIT WINDOW	*netrw-o* *netrw-horiz* {{{2

Normally one enters a file or directory using the <cr>.  However, the "o" map
allows one to open a new window to hold the new directory listing or file.  A
horizontal split is used.  (for vertical splitting, see |netrw-v|)

Normally, the o key splits the window horizontally with the new window and
cursor at the top.

Associated setting variables: |g:netrw_alto| |g:netrw_winsize|

Related topics:
	|netrw-ctrl-r|	|netrw-o|	|netrw-p|
	|netrw-P|	|netrw-t|	|netrw-v|
Associated setting variables:
   |g:netrw_alto|    control above/below splitting
   |g:netrw_winsize| control initial sizing

BROWSING WITH A NEW TAB				*netrw-t* {{{2

Normally one enters a file or directory using the <cr>.  The "t" map
allows one to open a new window holding the new directory listing or file in
a new tab.

If you'd like to have the new listing in a background tab, use |gT|.

Related topics:
	|netrw-ctrl-r|	|netrw-o|	|netrw-p|
	|netrw-P|	|netrw-t|	|netrw-v|
Associated setting variables:
   |g:netrw_winsize| control initial sizing

BROWSING WITH A VERTICALLY SPLIT WINDOW			*netrw-v* {{{2

Normally one enters a file or directory using the <cr>.  However, the "v" map
allows one to open a new window to hold the new directory listing or file.  A
vertical split is used.  (for horizontal splitting, see |netrw-o|)

Normally, the v key splits the window vertically with the new window and
cursor at the left.

There is only one tree listing buffer; using "v" on a displayed subdirectory
will split the screen, but the same buffer will be shown twice.

Related topics:
	|netrw-ctrl-r|	|netrw-o|	|netrw-p|
	|netrw-P|	|netrw-t|	|netrw-v|
Associated setting variables:
   |g:netrw_altv|    control right/left splitting
   |g:netrw_winsize| control initial sizing


BROWSING USING A GVIM SERVER			*netrw-ctrl-r* {{{2

One may keep a browsing gvim separate from the gvim being used to edit.
Use the <c-r> map on a file (not a directory) in the netrw browser, and it
will use a gvim server (see |g:netrw_servername|).  Subsequent use of <cr>
(see |netrw-cr|) will re-use that server for editing files.

Related topics:
	|netrw-ctrl-r|	|netrw-o|	|netrw-p|
	|netrw-P|	|netrw-t|	|netrw-v|
Associated setting variables:
	|g:netrw_servername|   : sets name of server
	|g:netrw_browse_split| : controls how <cr> will open files


CHANGE LISTING STYLE  (THIN LONG WIDE TREE)			*netrw-i* {{{2

The "i" map cycles between the thin, long, wide, and tree listing formats.

The thin listing format gives just the files' and directories' names.

The long listing is either based on the "ls" command via ssh for remote
directories or displays the filename, file size (in bytes), and the time and
date of last modification for local directories.  With the long listing
format, netrw is not able to recognize filenames which have trailing spaces.
Use the thin listing format for such files.

The wide listing format uses two or more contiguous spaces to delineate
filenames; when using that format, netrw won't be able to recognize or use
filenames which have two or more contiguous spaces embedded in the name or any
trailing spaces.  The thin listing format will, however, work with such files.
The wide listing format is the most compact.

The tree listing format has a top directory followed by files and directories
preceded by one or more "|"s, which indicate the directory depth.  One may
open and close directories by pressing the <cr> key while atop the directory
name.

One may make a preferred listing style your default; see |g:netrw_liststyle|.
As an example, by putting the following line in your .vimrc, >
	let g:netrw_liststyle= 3
the tree style will become your default listing style.

One typical way to use the netrw tree display is to: >

	vim .
	(use i until a tree display shows)
	navigate to a file
	v  (edit as desired in vertically split window)
	ctrl-w h  (to return to the netrw listing)
	P (edit newly selected file in the previous window)
	ctrl-w h  (to return to the netrw listing)
	P (edit newly selected file in the previous window)
	...etc...
<
Associated setting variables: |g:netrw_liststyle| |g:netrw_maxfilenamelen|
                              |g:netrw_timefmt|   |g:netrw_list_cmd|

CHANGE FILE PERMISSION						*netrw-gp* {{{2

"gp" will ask you for a new permission for the file named under the cursor.
Currently, this only works for local files.

Associated setting variables: |g:netrw_chgperm|


CHANGING TO A BOOKMARKED DIRECTORY			*netrw-gb*  {{{2

To change directory back to a bookmarked directory, use

	{cnt}gb

Any count may be used to reference any of the bookmarks.
Note that |netrw-qb| shows both bookmarks and history; to go
to a location stored in the history see |netrw-u| and |netrw-U|.

Related Topics:
	|netrw-mB| how to delete bookmarks
	|netrw-mb| how to make a bookmark
	|netrw-qb| how to list bookmarks


CHANGING TO A PREDECESSOR DIRECTORY		*netrw-u* *netrw-updir* {{{2

Every time you change to a new directory (new for the current session),
netrw will save the directory in a recently-visited directory history
list (unless |g:netrw_dirhistmax| is zero; by default, it's ten).  With the
"u" map, one can change to an earlier directory (predecessor).  To do
the opposite, see |netrw-U|.

The "u" map also accepts counts to go back in the history several slots.
For your convenience, qb (see |netrw-qb|) lists the history number which may
be used in that count.

						*.netrwhist*
See |g:netrw_dirhistmax| for how to control the quantity of history stack
slots.  The file ".netrwhist" holds history when netrw (and vim) is not
active.  By default, it's stored on the first directory on the user's
|'runtimepath'|.

Related Topics:
	|netrw-U| changing to a successor directory
	|g:netrw_home| controls where .netrwhist is kept


CHANGING TO A SUCCESSOR DIRECTORY		*netrw-U* *netrw-downdir* {{{2

With the "U" map, one can change to a later directory (successor).
This map is the opposite of the "u" map. (see |netrw-u|)  Use the
qb map to list both the bookmarks and history. (see |netrw-qb|)

The "U" map also accepts counts to go forward in the history several slots.

See |g:netrw_dirhistmax| for how to control the quantity of history stack
slots.


CHANGING TREE TOP			*netrw-ntree*  *:Ntree*  *netrw-gn* {{{2

One may specify a new tree top for tree listings using >

	:Ntree [dirname]

Without a "dirname", the current line is used (and any leading depth
information is elided).
With a "dirname", the specified directory name is used.

The "gn" map will take the word below the cursor and use that for
changing the top of the tree listing.


NETRW CLEAN					*netrw-clean* *:NetrwClean* {{{2

With NetrwClean one may easily remove netrw from one's home directory;
more precisely, from the first directory on your |'runtimepath'|.

With NetrwClean!, netrw will attempt to remove netrw from all directories on
your |'runtimepath'|.  Of course, you have to have write/delete permissions
correct to do this.

With either form of the command, netrw will first ask for confirmation
that the removal is in fact what you want to do.  If netrw doesn't have
permission to remove a file, it will issue an error message.

						*netrw-gx*
CUSTOMIZING BROWSING WITH A SPECIAL HANDLER	*netrw-x* *netrw-handler* {{{2
						(also see |netrw_filehandler|)

Certain files, such as html, gif, jpeg, (word/office) doc, etc, files, are
best seen with a special handler (ie. a tool provided with your computer's
operating system).  Netrw allows one to invoke such special handlers by: >

	* when Exploring, hit the "x" key
	* when editing, hit gx with the cursor atop the special filename
<	  (latter not available if the |g:netrw_nogx| variable exists)

Netrw determines which special handler by the following method:

  * if |g:netrw_browsex_viewer| exists, then it will be used to attempt to
    view files.  Examples of useful settings (place into your <.vimrc>): >

	:let g:netrw_browsex_viewer= "kfmclient exec"
<   or >
	:let g:netrw_browsex_viewer= "xdg-open"
<
    If g:netrw_browsex_viewer == '-', then netrwFileHandlers#Invoke() will be
    used instead (see |netrw_filehandler|).

  * for Windows 32 or 64, the url and FileProtocolHandler dlls are used.
  * for Gnome (with gnome-open): gnome-open is used.
  * for KDE (with kfmclient)   : kfmclient is used
  * for Mac OS X               : open is used.
  * otherwise the netrwFileHandler plugin is used.

The file's suffix is used by these various approaches to determine an
appropriate application to use to "handle" these files.  Such things as
OpenOffice (*.sfx), visualization (*.jpg, *.gif, etc), and PostScript (*.ps,
*.eps) can be handled.

The gx mapping extends to all buffers; apply "gx" while atop a word and netrw
will apply a special handler to it (like "x" works when in a netrw buffer).
One may also use visual mode (see |visual-start|) to select the text that the
special handler will use.  Normally gx uses expand("<cfile>") to pick up the
text under the cursor; one may change what |expand()| uses via the
|g:netrw_gx| variable.  Alternatively, one may select the text to be used by
gx via first making a visual selection (see |visual-block|) or by changing
the |'isfname'| option (which is global, so netrw doesn't modify it).

Associated setting variables:
	|g:netrw_gx|	control how gx picks up the text under the cursor
	|g:netrw_nogx|	prevent gx map while editing
	|g:netrw_suppress_gx_mesg| controls gx's suppression of browser messages

							*netrw_filehandler*

When |g:netrw_browsex_viewer| exists and is "-", then netrw will attempt to
handle the special file with a vim function.  The "x" map applies a function
to a file, based on its extension.  Of course, the handler function must exist
for it to be called!
>
 Ex. mypgm.html   x -> NFH_html("scp://user@host/some/path/mypgm.html")

<	Users may write their own netrw File Handler functions to
	support more suffixes with special handling.  See
	<autoload/netrwFileHandlers.vim> for examples on how to make
	file handler functions.   As an example: >

	" NFH_suffix(filename)
	fun! NFH_suffix(filename)
	..do something special with filename..
	endfun
<
These functions need to be defined in some file in your .vim/plugin
(vimfiles\plugin) directory.  Vim's function names may not have punctuation
characters (except for the underscore) in them.  To support suffices that
contain such characters, netrw will first convert the suffix using the
following table: >

    @ -> AT       ! -> EXCLAMATION    % -> PERCENT
    : -> COLON    = -> EQUAL          ? -> QUESTION
    , -> COMMA    - -> MINUS          ; -> SEMICOLON
    $ -> DOLLAR   + -> PLUS           ~ -> TILDE
<
So, for example: >

	file.rcs,v  ->  NFH_rcsCOMMAv()
<
If more such translations are necessary, please send me email: >
		NdrOchip at ScampbellPfamily.AbizM - NOSPAM
with a request.

Associated setting variable: |g:netrw_browsex_viewer|

							*netrw-curdir*
DELETING BOOKMARKS					*netrw-mB* {{{2

To delete a bookmark, use >

	{cnt}mB

If there are marked files, then mB will remove them from the
bookmark list.

Alternatively, one may use :NetrwMB! (see |netrw-:NetrwMB|). >

	:NetrwMB! [files/directories]

Related Topics:
	|netrw-gb| how to return (go) to a bookmark
	|netrw-mb| how to make a bookmark
	|netrw-qb| how to list bookmarks


DELETING FILES OR DIRECTORIES	*netrw-delete* *netrw-D* *netrw-del* {{{2

If files have not been marked with |netrw-mf|:   (local marked file list)

    Deleting/removing files and directories involves moving the cursor to the
    file/directory to be deleted and pressing "D".  Directories must be empty
    first before they can be successfully removed.  If the directory is a
    softlink to a directory, then netrw will make two requests to remove the
    directory before succeeding.  Netrw will ask for confirmation before doing
    the removal(s).  You may select a range of lines with the "V" command
    (visual selection), and then pressing "D".

If files have been marked with |netrw-mf|:   (local marked file list)

    Marked files (and empty directories) will be deleted; again, you'll be
    asked to confirm the deletion before it actually takes place.

A further approach is to delete files which match a pattern.

    * use  :MF pattern  (see |netrw-:MF|); then press "D".

    * use mr (see |netrw-mr|) which will prompt you for pattern.
      This will cause the matching files to be marked.  Then,
      press "D".

The |g:netrw_rm_cmd|, |g:netrw_rmf_cmd|, and |g:netrw_rmdir_cmd| variables are
used to control the attempts to remove remote files and directories.  The
g:netrw_rm_cmd is used with files, and its default value is:

	g:netrw_rm_cmd: ssh HOSTNAME rm

The g:netrw_rmdir_cmd variable is used to support the removal of directories.
Its default value is:

	|g:netrw_rmdir_cmd|: ssh HOSTNAME rmdir

If removing a directory fails with g:netrw_rmdir_cmd, netrw then will attempt
to remove it again using the g:netrw_rmf_cmd variable.  Its default value is:

	|g:netrw_rmf_cmd|: ssh HOSTNAME rm -f

Related topics: |netrw-d|
Associated setting variable: |g:netrw_localrmdir| |g:netrw_rm_cmd|
                             |g:netrw_rmdir_cmd|   |g:netrw_ssh_cmd|


*netrw-explore*  *netrw-hexplore* *netrw-nexplore* *netrw-pexplore*
*netrw-rexplore* *netrw-sexplore* *netrw-texplore* *netrw-vexplore* *netrw-lexplore*
DIRECTORY EXPLORATION COMMANDS  {{{2

     :[N]Explore[!]  [dir]... Explore directory of current file      *:Explore*
     :[N]Hexplore[!] [dir]... Horizontal Split & Explore             *:Hexplore*
     :[N]Lexplore[!] [dir]... Left Explorer Toggle                   *:Lexplore*
     :[N]Sexplore[!] [dir]... Split&Explore current file's directory *:Sexplore*
     :[N]Vexplore[!] [dir]... Vertical   Split & Explore             *:Vexplore*
     :Texplore       [dir]... Tab & Explore                          *:Texplore*
     :Rexplore            ... Return to/from Explorer                *:Rexplore*

     Used with :Explore **/pattern : (also see |netrw-starstar|)
     :Nexplore............. go to next matching file                *:Nexplore*
     :Pexplore............. go to previous matching file            *:Pexplore*

						*netrw-:Explore*
:Explore  will open the local-directory browser on the current file's
          directory (or on directory [dir] if specified).  The window will be
	  split only if the file has been modified and |'hidden'| is not set,
	  otherwise the browsing window will take over that window.  Normally
	  the splitting is taken horizontally.
	  Also see: |netrw-:Rexplore|
:Explore! is like :Explore, but will use vertical splitting.

						*netrw-:Hexplore*
:Hexplore  [dir] does an :Explore with |:belowright| horizontal splitting.
:Hexplore! [dir] does an :Explore with |:aboveleft|  horizontal splitting.

						*netrw-:Lexplore*
:[N]Lexplore [dir] toggles a full height Explorer window on the left hand side
	  of the current tab.  It will open a netrw window on the current
	  directory if [dir] is omitted; a :Lexplore [dir] will show the
	  specified directory in the left-hand side browser display no matter
	  from which window the command is issued.

	  By default, :Lexplore will change an uninitialized |g:netrw_chgwin|
	  to 2; edits will thus preferentially be made in window#2.

	  The [N] specifies a |g:netrw_winsize| just for the new :Lexplore
	  window.

	  Those who like this method often also often like tree style displays;
	  see |g:netrw_liststyle|.

	  Also see: |netrw-C|           |g:netrw_browse_split|   |g:netrw_wiw|
		    |netrw-p| |netrw-P|   |g:netrw_chgwin|
		    |netrw-c-tab|       |g:netrw_winsize|

:[N]Lexplore! is like :Lexplore, except that the full-height Explorer window
	  will open on the right hand side and an uninitialized |g:netrw_chgwin|
	  will be set to 1.

						*netrw-:Sexplore*
:[N]Sexplore will always split the window before invoking the local-directory
	  browser.  As with Explore, the splitting is normally done
	  horizontally.
:[N]Sexplore! [dir] is like :Sexplore, but the splitting will be done vertically.

						*netrw-:Texplore*
:Texplore  [dir] does a |:tabnew| before generating the browser window

						*netrw-:Vexplore*
:[N]Vexplore  [dir] does an :Explore with |:leftabove|  vertical splitting.
:[N]Vexplore! [dir] does an :Explore with |:rightbelow| vertical splitting.

The optional parameters are:

 [N]: This parameter will override |g:netrw_winsize| to specify the quantity of
      rows and/or columns the new explorer window should have.
      Otherwise, the |g:netrw_winsize| variable, if it has been specified by the
      user, is used to control the quantity of rows and/or columns new
      explorer windows should have.

 [dir]: By default, these explorer commands use the current file's directory.
        However, one may explicitly provide a directory (path) to use instead;
	ie. >

	:Explore /some/path
<
						*netrw-:Rexplore*
:Rexplore  This command is a little different from the other Explore commands
	   as it doesn't necessarily open an Explorer window.

	   Return to Explorer~
	   When one edits a file using netrw which can occur, for example,
	   when pressing <cr> while the cursor is atop a filename in a netrw
	   browser window, a :Rexplore issued while editing that file will
	   return the display to that of the last netrw browser display in
	   that window.

	   Return from Explorer~
	   Conversely, when one is editing a directory, issuing a :Rexplore
	   will return to editing the file that was last edited in that
	   window.

	   The <2-leftmouse> map (which is only available under gvim and
	   cooperative terms) does the same as :Rexplore.

Also see: |g:netrw_alto| |g:netrw_altv| |g:netrw_winsize|


*netrw-star* *netrw-starpat* *netrw-starstar* *netrw-starstarpat* *netrw-grep*
EXPLORING WITH STARS AND PATTERNS {{{2

When Explore, Sexplore, Hexplore, or Vexplore are used with one of the
following four patterns Explore generates a list of files which satisfy the
request for the local file system.  These exploration patterns will not work
with remote file browsing.

    */filepat	files in current directory which satisfy filepat
    **/filepat	files in current directory or below which satisfy the
		file pattern
    *//pattern	files in the current directory which contain the
		pattern (vimgrep is used)
    **//pattern	files in the current directory or below which contain
		the pattern (vimgrep is used)
<
The cursor will be placed on the first file in the list.  One may then
continue to go to subsequent files on that list via |:Nexplore| or to
preceding files on that list with |:Pexplore|.  Explore will update the
directory and place the cursor appropriately.

A plain >
	:Explore
will clear the explore list.

If your console or gui produces recognizable shift-up or shift-down sequences,
then you'll likely find using shift-downarrow and shift-uparrow convenient.
They're mapped by netrw as follows:

	<s-down>  == Nexplore, and
	<s-up>    == Pexplore.

As an example, consider
>
	:Explore */*.c
	:Nexplore
	:Nexplore
	:Pexplore
<
The status line will show, on the right hand side of the status line, a
message like "Match 3 of 20".

Associated setting variables:
	|g:netrw_keepdir|          |g:netrw_browse_split|
	|g:netrw_fastbrowse|       |g:netrw_ftp_browse_reject|
	|g:netrw_ftp_list_cmd|     |g:netrw_ftp_sizelist_cmd|
	|g:netrw_ftp_timelist_cmd| |g:netrw_list_cmd|
	|g:netrw_liststyle|


DISPLAYING INFORMATION ABOUT FILE				*netrw-qf* {{{2

With the cursor atop a filename, pressing "qf" will reveal the file's size
and last modification timestamp.  Currently this capability is only available
for local files.


EDIT FILE OR DIRECTORY HIDING LIST	*netrw-ctrl-h* *netrw-edithide* {{{2

The "<ctrl-h>" map brings up a requestor allowing the user to change the
file/directory hiding list contained in |g:netrw_list_hide|.  The hiding list
consists of one or more patterns delimited by commas.  Files and/or
directories satisfying these patterns will either be hidden (ie. not shown) or
be the only ones displayed (see |netrw-a|).

The "gh" mapping (see |netrw-gh|) quickly alternates between the usual
hiding list and the hiding of files or directories that begin with ".".

As an example, >
	let g:netrw_list_hide= '\(^\|\s\s\)\zs\.\S\+'
Effectively, this makes the effect of a |netrw-gh| command the initial setting.
What it means:

	\(^\|\s\s\)   : if the line begins with the following, -or-
	                two consecutive spaces are encountered
	\zs           : start the hiding match now
	\.            : if it now begins with a dot
	\S\+          : and is followed by one or more non-whitespace
	                characters

Associated setting variables: |g:netrw_hide| |g:netrw_list_hide|
Associated topics: |netrw-a| |netrw-gh| |netrw-mh|

					*netrw-sort-sequence*
EDITING THE SORTING SEQUENCE		*netrw-S* *netrw-sortsequence* {{{2

When "Sorted by" is name, one may specify priority via the sorting sequence
(g:netrw_sort_sequence).  The sorting sequence typically prioritizes the
name-listing by suffix, although any pattern will do.  Patterns are delimited
by commas.  The default sorting sequence is (all one line):

For Unix: >
	'[\/]$,\<core\%(\.\d\+\)\=,\.[a-np-z]$,\.h$,\.c$,\.cpp$,*,\.o$,\.obj$,
	\.info$,\.swp$,\.bak$,\~$'
<
Otherwise: >
	'[\/]$,\.[a-np-z]$,\.h$,\.c$,\.cpp$,*,\.o$,\.obj$,\.info$,
	\.swp$,\.bak$,\~$'
<
The lone * is where all filenames not covered by one of the other patterns
will end up.  One may change the sorting sequence by modifying the
g:netrw_sort_sequence variable (either manually or in your <.vimrc>) or by
using the "S" map.

Related topics:               |netrw-s|               |netrw-S|
Associated setting variables: |g:netrw_sort_sequence| |g:netrw_sort_options|


EXECUTING FILE UNDER CURSOR VIA SYSTEM()			*netrw-X* {{{2

Pressing X while the cursor is atop an executable file will yield a prompt
using the filename asking for any arguments.  Upon pressing a [return], netrw
will then call |system()| with that command and arguments.  The result will
be displayed by |:echomsg|, and so |:messages| will repeat display of the
result.  Ansi escape sequences will be stripped out.


FORCING TREATMENT AS A FILE OR DIRECTORY	*netrw-gd* *netrw-gf* {{{2

Remote symbolic links (ie. those listed via ssh or ftp) are problematic
in that it is difficult to tell whether they link to a file or to a
directory.

To force treatment as a file: use >
	gf
<
To force treatment as a directory: use >
	gd
<

GOING UP							*netrw--* {{{2

To go up a directory, press "-" or press the <cr> when atop the ../ directory
entry in the listing.

Netrw will use the command in |g:netrw_list_cmd| to perform the directory
listing operation after changing HOSTNAME to the host specified by the
user-prpvided url.  By default netrw provides the command as: >

	ssh HOSTNAME ls -FLa
<
where the HOSTNAME becomes the [user@]hostname as requested by the attempt to
read.  Naturally, the user may override this command with whatever is
preferred.  The NetList function which implements remote browsing
expects that directories will be flagged by a trailing slash.


HIDING FILES OR DIRECTORIES			*netrw-a* *netrw-hiding* {{{2

Netrw's browsing facility allows one to use the hiding list in one of three
ways: ignore it, hide files which match, and show only those files which
match.

If no files have been marked via |netrw-mf|:

The "a" map allows the user to cycle through the three hiding modes.

The |g:netrw_list_hide| variable holds a comma delimited list of patterns
based on regular expressions (ex. ^.*\.obj$,^\.) which specify the hiding list.
(also see |netrw-ctrl-h|)  To set the hiding list, use the <c-h> map.  As an
example, to hide files which begin with a ".", one may use the <c-h> map to
set the hiding list to '^\..*' (or one may put let g:netrw_list_hide= '^\..*'
in one's <.vimrc>).  One may then use the "a" key to show all files, hide
matching files, or to show only the matching files.

	Example: \.[ch]$
		This hiding list command will hide/show all *.c and *.h files.

	Example: \.c$,\.h$
		This hiding list command will also hide/show all *.c and *.h
		files.

Don't forget to use the "a" map to select the mode (normal/hiding/show) you
want!

If files have been marked using |netrw-mf|, then this command will:

  if showing all files or non-hidden files:
   modify the g:netrw_list_hide list by appending the marked files to it
   and showing only non-hidden files.

  else if showing hidden files only:
   modify the g:netrw_list_hide list by removing the marked files from it
   and showing only non-hidden files.
  endif

					*netrw-gh* *netrw-hide*
As a quick shortcut, one may press >
	gh
to toggle between hiding files which begin with a period (dot) and not hiding
them.

Associated setting variables: |g:netrw_list_hide|  |g:netrw_hide|
Associated topics: |netrw-a| |netrw-ctrl-h| |netrw-mh|

					*netrw-gitignore*
Netrw provides a helper function 'netrw_gitignore#Hide()' that, when used with
|g:netrw_list_hide| automatically hides all git-ignored files.

'netrw_gitignore#Hide' searches for patterns in the following files: >

	'./.gitignore'
	'./.git/info/exclude'
	global gitignore file: `git config --global core.excludesfile`
	system gitignore file: `git config --system core.excludesfile`
<
Files that do not exist, are ignored.
Git-ignore patterns are taken from existing files, and converted to patterns for
hiding files. For example, if you had '*.log' in your '.gitignore' file, it
would be converted to '.*\.log'.

To use this function, simply assign its output to |g:netrw_list_hide| option.  >

	Example: let g:netrw_list_hide= netrw_gitignore#Hide()
		Git-ignored files are hidden in Netrw.

	Example: let g:netrw_list_hide= netrw_gitignore#Hide('my_gitignore_file')
		Function can take additional files with git-ignore patterns.

	Example: g:netrw_list_hide= netrw_gitignore#Hide() . '.*\.swp$'
		Combining 'netrw_gitignore#Hide' with custom patterns.
<

IMPROVING BROWSING			*netrw-listhack* *netrw-ssh-hack* {{{2

Especially with the remote directory browser, constantly entering the password
is tedious.

For Linux/Unix systems, the book "Linux Server Hacks - 100 industrial strength
tips & tools" by Rob Flickenger (O'Reilly, ISBN 0-596-00461-3) gives a tip
for setting up no-password ssh and scp and discusses associated security
issues.  It used to be available at http://hacks.oreilly.com/pub/h/66 ,
but apparently that address is now being redirected to some "hackzine".
I'll attempt a summary based on that article and on a communication from
Ben Schmidt:

	1. Generate a public/private key pair on the local machine
	   (ssh client): >
		ssh-keygen -t rsa
		(saving the file in ~/.ssh/id_rsa as prompted)
<
	2. Just hit the <CR> when asked for passphrase (twice) for no
	   passphrase.  If you do use a passphrase, you will also need to use
	   ssh-agent so you only have to type the passphrase once per session.
	   If you don't use a passphrase, simply logging onto your local
	   computer or getting access to the keyfile in any way will suffice
	   to access any ssh servers which have that key authorized for login.

	3. This creates two files: >
		~/.ssh/id_rsa
		~/.ssh/id_rsa.pub
<
	4. On the target machine (ssh server): >
		cd
		mkdir -p .ssh
		chmod 0700 .ssh
<
	5. On your local machine (ssh client): (one line) >
		ssh {serverhostname}
		  cat '>>' '~/.ssh/authorized_keys2' < ~/.ssh/id_rsa.pub
<
	   or, for OpenSSH, (one line) >
		ssh {serverhostname}
		  cat '>>' '~/.ssh/authorized_keys' < ~/.ssh/id_rsa.pub
<
You can test it out with >
	ssh {serverhostname}
and you should be log onto the server machine without further need to type
anything.

If you decided to use a passphrase, do: >
	ssh-agent $SHELL
	ssh-add
	ssh {serverhostname}
You will be prompted for your key passphrase when you use ssh-add, but not
subsequently when you use ssh.  For use with vim, you can use >
	ssh-agent vim
and, when next within vim, use >
	:!ssh-add
Alternatively, you can apply ssh-agent to the terminal you're planning on
running vim in: >
	ssh-agent xterm &
and do ssh-add whenever you need.

For Windows, folks on the vim mailing list have mentioned that Pageant helps
with avoiding the constant need to enter the password.

Kingston Fung wrote about another way to avoid constantly needing to enter
passwords:

    In order to avoid the need to type in the password for scp each time, you
    provide a hack in the docs to set up a non password ssh account. I found a
    better way to do that: I can use a regular ssh account which uses a
    password to access the material without the need to key-in the password
    each time. It's good for security and convenience. I tried ssh public key
    authorization + ssh-agent, implementing this, and it works! Here are two
    links with instructions:

    http://www.ibm.com/developerworks/library/l-keyc2/
    http://sial.org/howto/openssh/publickey-auth/


    Ssh hints:

	Thomer Gil has provided a hint on how to speed up netrw+ssh:
	    http://thomer.com/howtos/netrw_ssh.html

	Alex Young has several hints on speeding ssh up:
	    http://usevim.com/2012/03/16/editing-remote-files/


LISTING BOOKMARKS AND HISTORY		*netrw-qb* *netrw-listbookmark* {{{2

Pressing "qb" (query bookmarks) will list both the bookmarked directories and
directory traversal history.

Related Topics:
	|netrw-gb| how to return (go) to a bookmark
	|netrw-mb| how to make a bookmark
	|netrw-mB| how to delete bookmarks
	|netrw-u|  change to a predecessor directory via the history stack
	|netrw-U|  change to a successor   directory via the history stack

MAKING A NEW DIRECTORY					*netrw-d* {{{2

With the "d" map one may make a new directory either remotely (which depends
on the global variable g:netrw_mkdir_cmd) or locally (which depends on the
global variable g:netrw_localmkdir).  Netrw will issue a request for the new
directory's name.  A bare <CR> at that point will abort the making of the
directory.  Attempts to make a local directory that already exists (as either
a file or a directory) will be detected, reported on, and ignored.

Related topics: |netrw-D|
Associated setting variables:	|g:netrw_localmkdir|   |g:netrw_mkdir_cmd|
				|g:netrw_remote_mkdir| |netrw-%|


MAKING THE BROWSING DIRECTORY THE CURRENT DIRECTORY	*netrw-c* {{{2

By default, |g:netrw_keepdir| is 1.  This setting means that the current
directory will not track the browsing directory. (done for backwards
compatibility with v6's file explorer).

Setting g:netrw_keepdir to 0 tells netrw to make vim's current directory
track netrw's browsing directory.

However, given the default setting for g:netrw_keepdir of 1 where netrw
maintains its own separate notion of the current directory, in order to make
the two directories the same, use the "c" map (just type c).  That map will
set Vim's notion of the current directory to netrw's current browsing
directory.

Associated setting variable: |g:netrw_keepdir|

MARKING FILES					*netrw-:MF*	*netrw-mf* {{{2
	(also see |netrw-mr|)

Netrw provides several ways to mark files:

	* One may mark files with the cursor atop a filename and
	  then pressing "mf".

	* With gvim, in addition one may mark files with
	  <s-leftmouse>. (see |netrw-mouse|)

	* One may use the :MF command, which takes a list of
	  files (for local directories, the list may include
	  wildcards -- see |glob()|) >

		:MF *.c
<
	  (Note that :MF uses |<f-args>| to break the line
	  at spaces)

	* Mark files using the |argument-list| (|netrw-mA|)

	* Mark files based upon a |location-list| (|netrw-qL|)

	* Mark files based upon the quickfix list (|netrw-qF|)
	  (|quickfix-error-lists|)

The following netrw maps make use of marked files:

    |netrw-a|	Hide marked files/directories
    |netrw-D|	Delete marked files/directories
    |netrw-ma|	Move marked files' names to |arglist|
    |netrw-mA|	Move |arglist| filenames to marked file list
    |netrw-mb|	Append marked files to bookmarks
    |netrw-mB|	Delete marked files from bookmarks
    |netrw-mc|	Copy marked files to target
    |netrw-md|	Apply vimdiff to marked files
    |netrw-me|	Edit marked files
    |netrw-mF|	Unmark marked files
    |netrw-mg|	Apply vimgrep to marked files
    |netrw-mm|	Move marked files to target
    |netrw-mp|	Print marked files
    |netrw-mt|	Set target for |netrw-mm| and |netrw-mc|
    |netrw-mT|	Generate tags using marked files
    |netrw-mv|	Apply vim command to marked files
    |netrw-mx|	Apply shell command to marked files
    |netrw-mX|	Apply shell command to marked files, en bloc
    |netrw-mz|	Compress/Decompress marked files
    |netrw-O|	Obtain marked files
    |netrw-R|	Rename marked files

One may unmark files one at a time the same way one marks them; ie. place
the cursor atop a marked file and press "mf".  This process also works
with <s-leftmouse> using gvim.  One may unmark all files by pressing
"mu" (see |netrw-mu|).

Marked files are highlighted using the "netrwMarkFile" highlighting group,
which by default is linked to "Identifier" (see Identifier under
|group-name|).  You may change the highlighting group by putting something
like >

	highlight clear netrwMarkFile
	hi link netrwMarkFile ..whatever..
<
into $HOME/.vim/after/syntax/netrw.vim .

If the mouse is enabled and works with your vim, you may use <s-leftmouse> to
mark one or more files.  You may mark multiple files by dragging the shifted
leftmouse.  (see |netrw-mouse|)

			*markfilelist* *global_markfilelist* *local_markfilelist*
All marked files are entered onto the global marked file list; there is only
one such list.  In addition, every netrw buffer also has its own buffer-local
marked file list; since netrw buffers are associated with specific
directories, this means that each directory has its own local marked file
list.  The various commands which operate on marked files use one or the other
of the marked file lists.

Known Problem: if one is using tree mode (|g:netrw_liststyle|) and several
directories have files with the same name,  then marking such a file will
result in all such files being highlighted as if they were all marked.  The
|markfilelist|, however, will only have the selected file in it.  This problem
is unlikely to be fixed.


UNMARKING FILES							*netrw-mF* {{{2
	(also see |netrw-mf|, |netrw-mu|)

The "mF" command will unmark all files in the current buffer.  One may also use
mf (|netrw-mf|) on a specific, already marked, file to unmark just that file.

MARKING FILES BY LOCATION LIST					*netrw-qL* {{{2
	(also see |netrw-mf|)

One may convert |location-list|s into a marked file list using "qL".
You may then proceed with commands such as me (|netrw-me|) to edit them.


MARKING FILES BY QUICKFIX LIST					*netrw-qF* {{{2
	(also see |netrw-mf|)

One may convert |quickfix-error-lists| into a marked file list using "qF".
You may then proceed with commands such as me (|netrw-me|) to edit them.
Quickfix error lists are generated, for example, by calls to |:vimgrep|.


MARKING FILES BY REGULAR EXPRESSION				*netrw-mr* {{{2
	(also see |netrw-mf|)

One may also mark files by pressing "mr"; netrw will then issue a prompt,
"Enter regexp: ".  You may then enter a shell-style regular expression such
as *.c$ (see |glob()|).  For remote systems, glob() doesn't work -- so netrw
converts "*" into ".*" (see |regexp|) and marks files based on that.  In the
future I may make it possible to use |regexp|s instead of glob()-style
expressions (yet-another-option).


MARKED FILES, ARBITRARY VIM COMMAND				*netrw-mv*  {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the local marked-file list)

The "mv" map causes netrw to execute an arbitrary vim command on each file on
the local marked file list, individually:

	* 1split
	* sil! keepalt e file
	* run vim command
	* sil! keepalt wq!

A prompt, "Enter vim command: ", will be issued to elicit the vim command
you wish used.


MARKED FILES, ARBITRARY SHELL COMMAND				*netrw-mx* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the local marked-file list)

Upon activation of the "mx" map, netrw will query the user for some (external)
command to be applied to all marked files.  All "%"s in the command will be
substituted with the name of each marked file in turn.  If no "%"s are in the
command, then the command will be followed by a space and a marked filename.

Example:
	(mark files)
	mx
	Enter command: cat

	The result is a series of shell commands:
	cat 'file1'
	cat 'file2'
	...


MARKED FILES, ARBITRARY SHELL COMMAND, EN BLOC 			*netrw-mX* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the global marked-file list)

Upon activation of the 'mX' map, netrw will query the user for some (external)
command to be applied to all marked files on the global marked file list.  The
"en bloc" means that one command will be executed on all the files at once: >

	command files

This approach is useful, for example, to select files and make a tarball: >

	(mark files)
	mX
	Enter command: tar cf mynewtarball.tar
<
The command that will be run with this example:

	tar cf mynewtarball.tar 'file1' 'file2' ...


MARKED FILES: ARGUMENT LIST				*netrw-ma* *netrw-mA*
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the global marked-file list)

Using ma, one moves filenames from the marked file list to the argument list.
Using mA, one moves filenames from the argument list to the marked file list.

See Also: |netrw-qF| |argument-list| |:args|


MARKED FILES: COMPRESSION AND DECOMPRESSION		*netrw-mz* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the local marked file list)

If any marked files are compressed,   then "mz" will decompress them.
If any marked files are decompressed, then "mz" will compress them
using the command specified by |g:netrw_compress|; by default,
that's "gzip".

For decompression, netrw uses a |Dictionary| of suffices and their
associated decompressing utilities; see |g:netrw_decompress|.

Remember that one can mark multiple files by regular expression
(see |netrw-mr|); this is particularly useful to facilitate compressing and
decompressing a large number of files.

Associated setting variables: |g:netrw_compress| |g:netrw_decompress|

MARKED FILES: COPYING						*netrw-mc* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (Uses the global marked file list)

Select a target directory with mt (|netrw-mt|).  Then change directory,
select file(s) (see |netrw-mf|), and press "mc".  The copy is done
from the current window (where one does the mf) to the target.

If one does not have a target directory set with |netrw-mt|, then netrw
will query you for a directory to copy to.

One may also copy directories and their contents (local only) to a target
directory.

Associated setting variables:
	|g:netrw_localcopycmd|
	|g:netrw_localcopydircmd|
	|g:netrw_ssh_cmd|

MARKED FILES: DIFF						*netrw-md* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the global marked file list)

Use |vimdiff| to visualize difference between selected files (two or
three may be selected for this).  Uses the global marked file list.

MARKED FILES: EDITING						*netrw-me* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the global marked file list)

The "me" command will place the marked files on the |arglist| and commence
editing them.  One may return the to explorer window with |:Rexplore|.
(use |:n| and |:p| to edit next and previous files in the arglist)

MARKED FILES: GREP						*netrw-mg* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the global marked file list)

The "mg" command will apply |:vimgrep| to the marked files.
The command will ask for the requested pattern; one may then enter: >

	/pattern/[g][j]
	! /pattern/[g][j]
	pattern
<
With /pattern/, editing will start with the first item on the |quickfix| list
that vimgrep sets up (see |:copen|, |:cnext|, |:cprevious|, |:cclose|).  The |:vimgrep|
command is in use, so without 'g' each line is added to quickfix list only
once; with 'g' every match is included.

With /pattern/j, "mg" will winnow the current marked file list to just those
marked files also possessing the specified pattern.  Thus, one may use >

	mr ...file-pattern...
	mg /pattern/j
<
to have a marked file list satisfying the file-pattern but also restricted to
files containing some desired pattern.


MARKED FILES: HIDING AND UNHIDING BY SUFFIX			*netrw-mh* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the local marked file list)

The "mh" command extracts the suffices of the marked files and toggles their
presence on the hiding list.  Please note that marking the same suffix
this way multiple times will result in the suffix's presence being toggled
for each file (so an even quantity of marked files having the same suffix
is the same as not having bothered to select them at all).

Related topics: |netrw-a| |g:netrw_list_hide|

MARKED FILES: MOVING						*netrw-mm* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the global marked file list)

	WARNING: moving files is more dangerous than copying them.
	A file being moved is first copied and then deleted; if the
	copy operation fails and the delete succeeds, you will lose
	the file.  Either try things out with unimportant files
	first or do the copy and then delete yourself using mc and D.
	Use at your own risk!

Select a target directory with mt (|netrw-mt|).  Then change directory,
select file(s) (see |netrw-mf|), and press "mm".  The move is done
from the current window (where one does the mf) to the target.

Associated setting variable: |g:netrw_localmovecmd| |g:netrw_ssh_cmd|

MARKED FILES: PRINTING						*netrw-mp* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the local marked file list)

When "mp" is used, netrw will apply the |:hardcopy| command to marked files.
What netrw does is open each file in a one-line window, execute hardcopy, then
close the one-line window.


MARKED FILES: SOURCING						*netrw-ms* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the local marked file list)

With "ms", netrw will source the marked files (using vim's |:source| command)


MARKED FILES: SETTING THE TARGET DIRECTORY			*netrw-mt* {{{2
     (See |netrw-mf| and |netrw-mr| for how to mark files)

Set the marked file copy/move-to target (see |netrw-mc| and |netrw-mm|):

  * If the cursor is atop a file name, then the netrw window's currently
    displayed directory is used for the copy/move-to target.

  * Also, if the cursor is in the banner, then the netrw window's currently
    displayed directory is used for the copy/move-to target.
    Unless the target already is the current directory.  In which case,
    typing "mf" clears the target.

  * However, if the cursor is atop a directory name, then that directory is
    used for the copy/move-to target

  * One may use the :MT [directory] command to set the target	*netrw-:MT*
    This command uses |<q-args>|, so spaces in the directory name are
    permitted without escaping.

  * With mouse-enabled vim or with gvim, one may select a target by using
    <c-leftmouse>

There is only one copy/move-to target at a time in a vim session; ie. the
target is a script variable (see |s:var|) and is shared between all netrw
windows (in an instance of vim).

When using menus and gvim, netrw provides a "Targets" entry which allows one
to pick a target from the list of bookmarks and history.

Related topics:
      Marking Files......................................|netrw-mf|
      Marking Files by Regular Expression................|netrw-mr|
      Marked Files: Target Directory Using Bookmarks.....|netrw-Tb|
      Marked Files: Target Directory Using History.......|netrw-Th|


MARKED FILES: TAGGING						*netrw-mT* {{{2
	    (See |netrw-mf| and |netrw-mr| for how to mark files)
		      (uses the global marked file list)

The "mT" mapping will apply the command in |g:netrw_ctags| (by default, it is
"ctags") to marked files.  For remote browsing, in order to create a tags file
netrw will use ssh (see |g:netrw_ssh_cmd|), and so ssh must be available for
this to work on remote systems.  For your local system, see |ctags| on how to
get a version.  I myself use hdrtags, currently available at
http://www.drchip.org/astronaut/src/index.html , and have >

	let g:netrw_ctags= "hdrtag"
<
in my <.vimrc>.

When a remote set of files are tagged, the resulting tags file is "obtained";
ie. a copy is transferred to the local system's directory.  The now local tags
file is then modified so that one may use it through the network.  The
modification made concerns the names of the files in the tags; each filename is
preceded by the netrw-compatible url used to obtain it.  When one subsequently
uses one of the go to tag actions (|tags|), the url will be used by netrw to
edit the desired file and go to the tag.

Associated setting variables: |g:netrw_ctags| |g:netrw_ssh_cmd|

MARKED FILES: TARGET DIRECTORY USING BOOKMARKS		*netrw-Tb* {{{2

Sets the marked file copy/move-to target.

The |netrw-qb| map will give you a list of bookmarks (and history).
One may choose one of the bookmarks to become your marked file
target by using [count]Tb (default count: 1).

Related topics:
      Copying files to target............................|netrw-mc|
      Listing Bookmarks and History......................|netrw-qb|
      Marked Files: Setting The Target Directory.........|netrw-mt|
      Marked Files: Target Directory Using History.......|netrw-Th|
      Marking Files......................................|netrw-mf|
      Marking Files by Regular Expression................|netrw-mr|
      Moving files to target.............................|netrw-mm|


MARKED FILES: TARGET DIRECTORY USING HISTORY			*netrw-Th* {{{2

Sets the marked file copy/move-to target.

The |netrw-qb| map will give you a list of history (and bookmarks).
One may choose one of the history entries to become your marked file
target by using [count]Th (default count: 0; ie. the current directory).

Related topics:
      Copying files to target............................|netrw-mc|
      Listing Bookmarks and History......................|netrw-qb|
      Marked Files: Setting The Target Directory.........|netrw-mt|
      Marked Files: Target Directory Using Bookmarks.....|netrw-Tb|
      Marking Files......................................|netrw-mf|
      Marking Files by Regular Expression................|netrw-mr|
      Moving files to target.............................|netrw-mm|


MARKED FILES: UNMARKING						*netrw-mu* {{{2
     (See |netrw-mf|, |netrw-mF|)

The "mu" mapping will unmark all currently marked files.  This command differs
from "mF" as the latter only unmarks files in the current directory whereas
"mu" will unmark global and all buffer-local marked files.
(see |netrw-mF|)


				*netrw-browser-settings*
NETRW BROWSER VARIABLES		*netrw-browser-options* *netrw-browser-var* {{{2

(if you're interested in the netrw file transfer settings, see |netrw-options|
 and |netrw-protocol|)

The <netrw.vim> browser provides settings in the form of variables which
you may modify; by placing these settings in your <.vimrc>, you may customize
your browsing preferences.  (see also: |netrw-settings|)
>
   ---				-----------
   Var				Explanation
   ---				-----------
<  *g:netrw_altfile*		some like |CTRL-^| to return to the last
				edited file.  Choose that by setting this
				parameter to 1.
				Others like |CTRL-^| to return to the
				netrw browsing buffer.  Choose that by setting
				this parameter to 0.
				 default: =0

  *g:netrw_alto*		change from above splitting to below splitting
				by setting this variable (see |netrw-o|)
				 default: =&sb           (see |'sb'|)

  *g:netrw_altv*		change from left splitting to right splitting
				by setting this variable (see |netrw-v|)
				 default: =&spr          (see |'spr'|)

  *g:netrw_banner*		enable/suppress the banner
				=0: suppress the banner
				=1: banner is enabled (default)

  *g:netrw_bannerbackslash*	if this variable exists and is not zero, the
				banner will be displayed with backslashes
				rather than forward slashes.

  *g:netrw_browse_split*	when browsing, <cr> will open the file by:
				=0: re-using the same window  (default)
				=1: horizontally splitting the window first
				=2: vertically   splitting the window first
				=3: open file in new tab
				=4: act like "P" (ie. open previous window)
				    Note that |g:netrw_preview| may be used
				    to get vertical splitting instead of
				    horizontal splitting.
				=[servername,tab-number,window-number]
				    Given a |List| such as this, a remote server
				    named by the "servername" will be used for
				    editing.  It will also use the specified tab
				    and window numbers to perform editing
				    (see |clientserver|, |netrw-ctrl-r|)
				This option does not affect |:Lexplore|
				windows.

				Related topics:
				    |g:netrw_alto|	|g:netrw_altv|
				    |netrw-C|		|netrw-cr|
				    |netrw-ctrl-r|

  *g:netrw_browsex_viewer*	specify user's preference for a viewer: >
					"kfmclient exec"
					"gnome-open"
<				If >
					"-"
<				is used, then netrwFileHandler() will look for
				a script/function to handle the given
				extension.  (see |netrw_filehandler|).

  *g:netrw_chgperm*		Unix/Linux: "chmod PERM FILENAME"
				Windows:    "cacls FILENAME /e /p PERM"
				Used to change access permission for a file.

  *g:netrw_compress*		="gzip"
				    Will compress marked files with this
				    command

  *g:Netrw_corehandler*		Allows one to specify something additional
				to do when handling <core> files via netrw's
				browser's "x" command (see |netrw-x|).  If
				present, g:Netrw_corehandler specifies
				either one or more function references
				(see |Funcref|).  (the capital g:Netrw...
				is required its holding a function reference)


  *g:netrw_ctags*		="ctags"
				The default external program used to create
				tags

  *g:netrw_cursor*		= 2 (default)
				This option controls the use of the
				|'cursorline'| (cul) and |'cursorcolumn'|
				(cuc) settings by netrw:

				Value   Thin-Long-Tree      Wide
				 =0      u-cul u-cuc      u-cul u-cuc
				 =1      u-cul u-cuc        cul u-cuc
				 =2        cul u-cuc        cul u-cuc
				 =3        cul u-cuc        cul   cuc
				 =4        cul   cuc        cul   cuc

				Where
				  u-cul : user's |'cursorline'|   setting used
				  u-cuc : user's |'cursorcolumn'| setting used
				  cul   : |'cursorline'|  locally set
				  cuc   : |'cursorcolumn'| locally set

  *g:netrw_decompress*		= { ".gz"  : "gunzip" ,
				    ".bz2" : "bunzip2" ,
				    ".zip" : "unzip" ,
				    ".tar" : "tar -xf"}
				  A dictionary mapping suffices to
				  decompression programs.

  *g:netrw_dirhistmax*            =10: controls maximum quantity of past
                                     history.  May be zero to supppress
				     history.
				     (related: |netrw-qb| |netrw-u| |netrw-U|)

  *g:netrw_dynamic_maxfilenamelen* =32: enables dynamic determination of
				    |g:netrw_maxfilenamelen|, which affects
				    local file long listing.

  *g:netrw_errorlvl*		=0: error levels greater than or equal to
				    this are permitted to be displayed
				    0: notes
				    1: warnings
				    2: errors

  *g:netrw_fastbrowse*		=0: slow speed directory browsing;
				    never re-uses directory listings;
				    always obtains directory listings.
				=1: medium speed directory browsing;
				    re-use directory listings only
				    when remote directory browsing.
				    (default value)
				=2: fast directory browsing;
				    only obtains directory listings when the
				    directory hasn't been seen before
				    (or |netrw-ctrl-l| is used).

				Fast browsing retains old directory listing
				buffers so that they don't need to be
				re-acquired.  This feature is especially
				important for remote browsing.  However, if
				a file is introduced or deleted into or from
				such directories, the old directory buffer
				becomes out-of-date.  One may always refresh
				such a directory listing with |netrw-ctrl-l|.
				This option gives the user the choice of
				trading off accuracy (ie. up-to-date listing)
				versus speed.

  *g:netrw_ffkeep*		(default: doesn't exist)
				If this variable exists and is zero, then
				netrw will not do a save and restore for
				|'fileformat'|.

  *g:netrw_fname_escape*	=' ?&;%'
				Used on filenames before remote reading/writing

  *g:netrw_ftp_browse_reject*	ftp can produce a number of errors and warnings
				that can show up as "directories" and "files"
				in the listing.  This pattern is used to
				remove such embedded messages.  By default its
				value is:
				 '^total\s\+\d\+$\|
				 ^Trying\s\+\d\+.*$\|
				 ^KERBEROS_V\d rejected\|
				 ^Security extensions not\|
				 No such file\|
				 : connect to address [0-9a-fA-F:]*
				 : No route to host$'

  *g:netrw_ftp_list_cmd*	options for passing along to ftp for directory
				listing.  Defaults:
				 unix or g:netrw_cygwin set: : "ls -lF"
				 otherwise                     "dir"


  *g:netrw_ftp_sizelist_cmd*	options for passing along to ftp for directory
				listing, sorted by size of file.
				Defaults:
				 unix or g:netrw_cygwin set: : "ls -slF"
				 otherwise                     "dir"

  *g:netrw_ftp_timelist_cmd*	options for passing along to ftp for directory
				listing, sorted by time of last modification.
				Defaults:
				 unix or g:netrw_cygwin set: : "ls -tlF"
				 otherwise                     "dir"

  *g:netrw_glob_escape*		='[]*?`{~$'  (unix)
				='[]*?`{$'  (windows
				These characters in directory names are
				escaped before applying glob()

  *g:netrw_gx*			="<cfile>"
 				This option controls how gx (|netrw-gx|) picks
				up the text under the cursor.  See |expand()|
				for possibilities.

  *g:netrw_hide*		Controlled by the "a" map (see |netrw-a|)
				=0 : show all
				=1 : show not-hidden files
				=2 : show hidden files only
				 default: =0

  *g:netrw_home*		The home directory for where bookmarks and
				history are saved (as .netrwbook and
				.netrwhist).
				 default: the first directory on the
				         |'runtimepath'|

  *g:netrw_keepdir*		=1 (default) keep current directory immune from
				   the browsing directory.
				=0 keep the current directory the same as the
				   browsing directory.
				The current browsing directory is contained in
				b:netrw_curdir (also see |netrw-c|)

  *g:netrw_keepj*		="keepj" (default) netrw attempts to keep the
				         |:jumps| table unaffected.
				=""      netrw will not use |:keepjumps| with
					 exceptions only for the
					 saving/restoration of position.

  *g:netrw_list_cmd*		command for listing remote directories
				 default: (if ssh is executable)
				          "ssh HOSTNAME ls -FLa"

 *g:netrw_list_cmd_options*	If this variable exists, then its contents are
				appended to the g:netrw_list_cmd.  For
				example, use "2>/dev/null" to get rid of banner
				messages on unix systems.


  *g:netrw_liststyle*		Set the default listing style:
                                = 0: thin listing (one file per line)
                                = 1: long listing (one file per line with time
				     stamp information and file size)
				= 2: wide listing (multiple files in columns)
				= 3: tree style listing

  *g:netrw_list_hide*		comma separated pattern list for hiding files
				Patterns are regular expressions (see |regexp|)
				There's some special support for git-ignore
				files: you may add the output from the helper
				function 'netrw_gitignore#Hide() automatically
				hiding all gitignored files.
				For more details see |netrw-gitignore|.

				Examples:
				 let g:netrw_list_hide= '.*\.swp$'
				 let g:netrw_list_hide= netrw_gitignore#Hide().'.*\.swp$'
				default: ""

  *g:netrw_localcopycmd*	="cp" Linux/Unix/MacOS/Cygwin
				="copy" Windows
				Copies marked files (|netrw-mf|) to target
				directory (|netrw-mt|, |netrw-mc|)

 *g:netrw_localcopydircmd*	="cp -R"	Linux/Unix/MacOS/Cygwin
				="xcopy /e /c /h/ /i /k"	Windows
				Copies directories to target directory.
				(|netrw-mc|, |netrw-mt|)

  *g:netrw_localmkdir*		command for making a local directory
				 default: "mkdir"

  *g:netrw_localmovecmd*	="mv" Linux/Unix/MacOS/Cygwin
				="move" Windows
				Moves marked files (|netrw-mf|) to target
				directory (|netrw-mt|, |netrw-mm|)

  *g:netrw_localrmdir*		remove directory command (rmdir)
				 default: "rmdir"

  *g:netrw_maxfilenamelen*	=32 by default, selected so as to make long
				    listings fit on 80 column displays.
				If your screen is wider, and you have file
				or directory names longer than 32 bytes,
				you may set this option to keep listings
				columnar.

  *g:netrw_mkdir_cmd*		command for making a remote directory
				via ssh  (also see |g:netrw_remote_mkdir|)
				 default: "ssh USEPORT HOSTNAME mkdir"

  *g:netrw_mousemaps*		  =1 (default) enables mouse buttons while
				   browsing to:
				     leftmouse       : open file/directory
				     shift-leftmouse : mark file
				     middlemouse     : same as P
				     rightmouse      : remove file/directory
				=0: disables mouse maps

  *g:netrw_nobeval*		doesn't exist (default)
				If this variable exists, then balloon
				evaluation will be suppressed
				(see |'ballooneval'|)

 *g:netrw_sizestyle*		not defined: actual bytes (default)
 				="b" : actual bytes       (default)
 				="h" : human-readable (ex. 5k, 4m, 3g)
				       uses 1000 base
 				="H" : human-readable (ex. 5K, 4M, 3G)
				       uses 1024 base
				The long listing (|netrw-i|) and query-file
				maps (|netrw-qf|) will display file size
				using the specified style.

  *g:netrw_usetab*		if this variable exists and is non-zero, then
				the <tab> map supporting shrinking/expanding a
				Lexplore or netrw window will be enabled.
				(see |netrw-c-tab|)

  *g:netrw_remote_mkdir*	command for making a remote directory
				via ftp  (also see |g:netrw_mkdir_cmd|)
				 default: "mkdir"

  *g:netrw_retmap*		if it exists and is set to one, then:
				 * if in a netrw-selected file, AND
				 * no normal-mode <2-leftmouse> mapping exists,
				then the <2-leftmouse> will be mapped for easy
				return to the netrw browser window.
				 example: click once to select and open a file,
				          double-click to return.

				Note that one may instead choose to:
				 * let g:netrw_retmap= 1, AND
				 * nmap <silent> YourChoice <Plug>NetrwReturn
				and have another mapping instead of
				<2-leftmouse> to invoke the return.

				You may also use the |:Rexplore| command to do
				the same thing.

				  default: =0

  *g:netrw_rm_cmd*		command for removing remote files
				 default: "ssh USEPORT HOSTNAME rm"

  *g:netrw_rmdir_cmd*		command for removing remote directories
				 default: "ssh USEPORT HOSTNAME rmdir"

  *g:netrw_rmf_cmd*		command for removing remote softlinks
				 default: "ssh USEPORT HOSTNAME rm -f"

  *g:netrw_servername*		use this variable to provide a name for
				|netrw-ctrl-r| to use for its server.
				 default: "NETRWSERVER"

  *g:netrw_sort_by*		sort by "name", "time", "size", or
  				"exten".
				 default: "name"

  *g:netrw_sort_direction*	sorting direction: "normal" or "reverse"
				 default: "normal"

  *g:netrw_sort_options*	sorting is done using |:sort|; this
				variable's value is appended to the
				sort command.  Thus one may ignore case,
				for example, with the following in your
				.vimrc: >
					let g:netrw_sort_options="i"
<				 default: ""

  *g:netrw_sort_sequence*	when sorting by name, first sort by the
				comma-separated pattern sequence.  Note that
				any filigree added to indicate filetypes
				should be accounted for in your pattern.
				 default: '[\/]$,*,\.bak$,\.o$,\.h$,
				           \.info$,\.swp$,\.obj$'

  *g:netrw_special_syntax*	If true, then certain files will be shown
				using special syntax in the browser:

					netrwBak     : *.bak
					netrwCompress: *.gz *.bz2 *.Z *.zip
					netrwData    : *.dat
					netrwHdr     : *.h
					netrwLib     : *.a *.so *.lib *.dll
					netrwMakefile: [mM]akefile *.mak
					netrwObj     : *.o *.obj
					netrwTags    : tags ANmenu ANtags
					netrwTilde   : *
					netrwTmp     : tmp* *tmp

				These syntax highlighting groups are linked
				to Folded or DiffChange by default
				(see |hl-Folded| and |hl-DiffChange|), but
				one may put lines like >
					hi link netrwCompress Visual
<				into one's <.vimrc> to use one's own
				preferences.  Alternatively, one may
				put such specifications into
				.vim/after/syntax/netrw.vim.

				As an example, I myself use a dark-background
				colorscheme with the following in
				.vim/after/syntax/netrw.vim: >

 hi netrwCompress term=NONE cterm=NONE gui=NONE ctermfg=10 guifg=green  ctermbg=0 guibg=black
 hi netrwData	  term=NONE cterm=NONE gui=NONE ctermfg=9 guifg=blue ctermbg=0 guibg=black
 hi netrwHdr	  term=NONE cterm=NONE,italic gui=NONE guifg=SeaGreen1
 hi netrwLex	  term=NONE cterm=NONE,italic gui=NONE guifg=SeaGreen1
 hi netrwYacc	  term=NONE cterm=NONE,italic gui=NONE guifg=SeaGreen1
 hi netrwLib	  term=NONE cterm=NONE gui=NONE ctermfg=14 guifg=yellow
 hi netrwObj	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 hi netrwTilde	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 hi netrwTmp	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 hi netrwTags	  term=NONE cterm=NONE gui=NONE ctermfg=12 guifg=red
 hi netrwDoc	  term=NONE cterm=NONE gui=NONE ctermfg=220 ctermbg=27 guifg=yellow2 guibg=Blue3
 hi netrwSymLink  term=NONE cterm=NONE gui=NONE ctermfg=220 ctermbg=27 guifg=grey60
<
  *g:netrw_ssh_browse_reject*	ssh can sometimes produce unwanted lines,
				messages, banners, and whatnot that one doesn't
				want masquerading as "directories" and "files".
				Use this pattern to remove such embedded
				messages.  By default its value is:
					 '^total\s\+\d\+$'

  *g:netrw_ssh_cmd*		One may specify an executable command
				to use instead of ssh for remote actions
				such as listing, file removal, etc.
				 default: ssh

 *g:netrw_suppress_gx_mesg*	=1 : browsers sometimes produce messages
				which are normally unwanted intermixed
				with the page.
				However, when using links, for example,
				those messages are what the browser produces.
				By setting this option to 0, netrw will not
				suppress browser messages.

  *g:netrw_tmpfile_escape*	=' &;'
				escape() is applied to all temporary files
				to escape these characters.

  *g:netrw_timefmt*		specify format string to vim's strftime().
				The default, "%c", is "the preferred date
				and time representation for the current
				locale" according to my manpage entry for
				strftime(); however, not all are satisfied
				with it.  Some alternatives:
				 "%a %d %b %Y %T",
				 " %a %Y-%m-%d  %I-%M-%S %p"
				 default: "%c"

  *g:netrw_use_noswf*		netrw normally avoids writing swapfiles
				for browser buffers.  However, under some
				systems this apparently is causing nasty
				ml_get errors to appear; if you're getting
				ml_get errors, try putting
				  let g:netrw_use_noswf= 0
				in your .vimrc.
				  default: 1

  *g:netrw_winsize*		specify initial size of new windows made with
				"o" (see |netrw-o|), "v" (see |netrw-v|),
				|:Hexplore| or |:Vexplore|.  The g:netrw_winsize
				is an integer describing the percentage of the
				current netrw buffer's window to be used for
				the new window.
				 If g:netrw_winsize is less than zero, then
				the absolute value of g:netrw_winsize lines
				or columns will be used for the new window.
				 If g:netrw_winsize is zero, then a normal
				split will be made (ie. |'equalalways'| will
				take effect, for example).
				 default: 50  (for 50%)

  *g:netrw_wiw*			=1 specifies the minimum window width to use
				when shrinking a netrw/Lexplore window
				(see |netrw-c-tab|).

  *g:netrw_xstrlen*		Controls how netrw computes string lengths,
				including multi-byte characters' string
				length. (thanks to N Weibull, T Mechelynck)
				=0: uses Vim's built-in strlen()
				=1: number of codepoints (Latin a + combining
				    circumflex is two codepoints)  (DEFAULT)
				=2: number of spacing codepoints (Latin a +
				    combining circumflex is one spacing
				    codepoint; a hard tab is one; wide and
				    narrow CJK are one each; etc.)
				=3: virtual length (counting tabs as anything
				    between 1 and |'tabstop'|, wide CJK as 2
				    rather than 1, Arabic alif as zero when
				    immediately preceded by lam, one
				    otherwise, etc)

  *g:NetrwTopLvlMenu*		This variable specifies the top level
				menu name; by default, it's "Netrw.".  If
				you wish to change this, do so in your
				.vimrc.

NETRW BROWSING AND OPTION INCOMPATIBILITIES	*netrw-incompatible* {{{2

Netrw has been designed to handle user options by saving them, setting the
options to something that's compatible with netrw's needs, and then restoring
them.  However, the autochdir option: >
	:set acd
is problematic.  Autochdir sets the current directory to that containing the
file you edit; this apparently also applies to directories.  In other words,
autochdir sets the current directory to that containing the "file" (even if
that "file" is itself a directory).

NETRW SETTINGS WINDOW				*netrw-settings-window* {{{2

With the NetrwSettings.vim plugin, >
	:NetrwSettings
will bring up a window with the many variables that netrw uses for its
settings.  You may change any of their values; when you save the file, the
settings therein will be used.  One may also press "?" on any of the lines for
help on what each of the variables do.

(also see: |netrw-browser-var| |netrw-protocol| |netrw-variables|)


==============================================================================
OBTAINING A FILE					*netrw-obtain* *netrw-O* {{{2

If there are no marked files:

    When browsing a remote directory, one may obtain a file under the cursor
    (ie.  get a copy on your local machine, but not edit it) by pressing the O
    key.

If there are marked files:

    The marked files will be obtained (ie. a copy will be transferred to your
    local machine, but not set up for editing).

Only ftp and scp are supported for this operation (but since these two are
available for browsing, that shouldn't be a problem).  The status bar will
then show, on its right hand side, a message like "Obtaining filename".  The
statusline will be restored after the transfer is complete.

Netrw can also "obtain" a file using the local browser.  Netrw's display
of a directory is not necessarily the same as Vim's "current directory",
unless |g:netrw_keepdir| is set to 0 in the user's <.vimrc>.  One may select
a file using the local browser (by putting the cursor on it) and pressing
"O" will then "obtain" the file; ie. copy it to Vim's current directory.

Related topics:
 * To see what the current directory is, use |:pwd|
 * To make the currently browsed directory the current directory, see |netrw-c|
 * To automatically make the currently browsed directory the current
   directory, see |g:netrw_keepdir|.

					*netrw-newfile* *netrw-createfile*
OPEN A NEW FILE IN NETRW'S CURRENT DIRECTORY		*netrw-%* {{{2

To open a new file in netrw's current directory, press "%".  This map
will query the user for a new filename; an empty file by that name will
be placed in the netrw's current directory (ie. b:netrw_curdir).

Related topics:               |netrw-d|


PREVIEW WINDOW				*netrw-p* *netrw-preview* {{{2

One may use a preview window by using the "p" key when the cursor is atop the
desired filename to be previewed.  The display will then split to show both
the browser (where the cursor will remain) and the file (see |:pedit|).  By
default, the split will be taken horizontally; one may use vertical splitting
if one has set |g:netrw_preview| first.

An interesting set of netrw settings is: >

	let g:netrw_preview   = 1
	let g:netrw_liststyle = 3
	let g:netrw_winsize   = 30

These will:

	1. Make vertical splitting the default for previewing files
	2. Make the default listing style "tree"
	3. When a vertical preview window is opened, the directory listing
	   will use only 30% of the columns available; the rest of the window
	   is used for the preview window.

	Related: if you like this idea, you may also find :Lexplore
	         (|netrw-:Lexplore|) or |g:netrw_chgwin| of interest

Also see: |g:netrw_chgwin| |netrw-P| |'previewwindow'| |CTRL-W_z| |:pclose|


PREVIOUS WINDOW					*netrw-P* *netrw-prvwin* {{{2

To edit a file or directory under the cursor in the previously used (last
accessed) window (see :he |CTRL-W_p|), press a "P".  If there's only one
window, then the one window will be horizontally split (by default).

If there's more than one window, the previous window will be re-used on
the selected file/directory.  If the previous window's associated buffer
has been modified, and there's only one window with that buffer, then
the user will be asked if s/he wishes to save the buffer first (yes,
no, or cancel).

Related Actions |netrw-cr| |netrw-o| |netrw-t| |netrw-v|
Associated setting variables:
   |g:netrw_alto|    control above/below splitting
   |g:netrw_altv|    control right/left splitting
   |g:netrw_preview| control horizontal vs vertical splitting
   |g:netrw_winsize| control initial sizing

Also see: |g:netrw_chgwin| |netrw-p|


REFRESHING THE LISTING		*netrw-refresh* *netrw-ctrl-l* *netrw-ctrl_l* {{{2

To refresh either a local or remote directory listing, press ctrl-l (<c-l>) or
hit the <cr> when atop the ./ directory entry in the listing.  One may also
refresh a local directory by using ":e .".


REVERSING SORTING ORDER		*netrw-r* *netrw-reverse* {{{2

One may toggle between normal and reverse sorting order by pressing the
"r" key.

Related topics:              |netrw-s|
Associated setting variable: |g:netrw_sort_direction|


RENAMING FILES OR DIRECTORIES	*netrw-move* *netrw-rename* *netrw-R* {{{2

If there are no marked files: (see |netrw-mf|)

    Renaming files and directories involves moving the cursor to the
    file/directory to be moved (renamed) and pressing "R".  You will then be
    queried for what you want the file/directory to be renamed to  You may select
    a range of lines with the "V" command (visual selection), and then
    press "R"; you will be queried for each file as to what you want it
    renamed to.

If there are marked files:  (see |netrw-mf|)

    Marked files will be renamed (moved).  You will be queried as above in
    order to specify where you want the file/directory to be moved.

    If you answer a renaming query with a "s/frompattern/topattern/", then
    subsequent files on the marked file list will be renamed by taking each
    name, applying that substitute, and renaming each file to the result.
    As an example : >

    	mr  [query: reply with *.c]
	R   [query: reply with s/^\(.*\)\.c$/\1.cpp/]
<
    This example will mark all *.c files and then rename them to *.cpp
    files.

    The ctrl-X character has special meaning for renaming files: >

    	<c-x>      : a single ctrl-x tells netrw to ignore the portion of the response
	             lying between the last '/' and the ctrl-x.

	<c-x><c-x> : a pair of contiguous ctrl-x's tells netrw to ignore any
		     portion of the string preceding the double ctrl-x's.
<
    WARNING:~

    Note that moving files is a dangerous operation; copies are safer.  That's
    because a "move" for remote files is actually a copy + delete -- and if
    the copy fails and the delete does not, you may lose the file.
    Use at your own risk.

The g:netrw_rename_cmd variable is used to implement remote renaming.  By
default its value is:

	ssh HOSTNAME mv

One may rename a block of files and directories by selecting them with
V (|linewise-visual|) when using thin style


SELECTING SORTING STYLE			*netrw-s* *netrw-sort* {{{2

One may select the sorting style by name, time, or (file) size.  The "s" map
allows one to circulate amongst the three choices; the directory listing will
automatically be refreshed to reflect the selected style.

Related topics:               |netrw-r| |netrw-S|
Associated setting variables: |g:netrw_sort_by| |g:netrw_sort_sequence|


SETTING EDITING WINDOW		*netrw-editwindow* *netrw-C* *netrw-:NetrwC* {{{2

One may select a netrw window for editing with the "C" mapping, using the
:NetrwC [win#] command, or by setting |g:netrw_chgwin| to the selected window
number.  Subsequent selection of a file to edit (|netrw-cr|) will use that
window.

	* C : by itself, will select the current window holding a netrw buffer
	  for editing via |netrw-cr|.  The C mapping is only available while in
	  netrw buffers.

	* [count]C : the count will be used as the window number to be used
	  for subsequent editing via |netrw-cr|.

	* :NetrwC will set |g:netrw_chgwin| to the current window

	* :NetrwC win#  will set |g:netrw_chgwin| to the specified window
	  number

Using >
	let g:netrw_chgwin= -1
will restore the default editing behavior
(ie. editing will use the current window).

Related topics:			|netrw-cr| |g:netrw_browse_split|
Associated setting variables:	|g:netrw_chgwin|


SHRINKING OR EXPANDING A NETRW OR LEXPLORE WINDOW	*netrw-c-tab* {{{2

The <c-tab> key will toggle a netrw or |:Lexplore| window's width,
but only if |g:netrw_usetab| exists and is non-zero (and, of course,
only if your terminal supports differentiating <c-tab> from a plain
<tab>).

  * If the current window is a netrw window, toggle its width
    (between |g:netrw_wiw| and its original width)

  * Else if there is a |:Lexplore| window in the current tab, toggle
    its width

  * Else bring up a |:Lexplore| window

If |g:netrw_usetab| exists or is zero, or if there is a pre-existing mapping
for <c-tab>, then the <c-tab> will not be mapped.  One may map something other
than a <c-tab>, too: (but you'll still need to have had g:netrw_usetab set) >

	nmap <unique> (whatever)	<Plug>NetrwShrink
<
Related topics:			|:Lexplore|
Associated setting variable:	|g:netrw_usetab|

vim: ft=help
