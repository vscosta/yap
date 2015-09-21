<html>
<head>
  <title>/var/www/vhosts/netmite.com/android/mydroid/bionic/libc/bionic/realpath.c</title>
</head>
<body bgcolor="#ffffff" text="#000000">
<pre>
<font color="#444444">/*
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */</font>

<font color="0000ff"><strong>#if defined(LIBC_SCCS) &amp;&amp; !defined(lint)</strong></font>
<strong>static</strong> <strong>char</strong> <font color="#2040a0">sccsid</font><font color="4444FF">[</font><font color="4444FF">]</font> <font color="4444FF">=</font> <font color="#008000">&quot;@(#)realpath.c	8.1 (Berkeley) 2/16/94&quot;</font><font color="4444FF">;</font>
<strong>static</strong> <strong>char</strong> <font color="#2040a0">rcsid</font><font color="4444FF">[</font><font color="4444FF">]</font> <font color="4444FF">=</font>
<font color="#008000">&quot;$FreeBSD: /repoman/r/ncvs/src/lib/libc/stdlib/realpath.c,v 1.6.2.1 2003/08/03 23:47:39 nectar Exp $&quot;</font><font color="4444FF">;</font>
<font color="0000ff"><strong>#endif<font color="#444444"> /* LIBC_SCCS and not lint */</font></strong></font>

<font color="0000ff"><strong>#include <font color="#008000">&lt;sys/param.h&gt;</font></strong></font>
<font color="0000ff"><strong>#include <font color="#008000">&lt;sys/stat.h&gt;</font></strong></font>

<font color="0000ff"><strong>#include <font color="#008000">&lt;errno.h&gt;</font></strong></font>
<font color="0000ff"><strong>#include <font color="#008000">&lt;fcntl.h&gt;</font></strong></font>
<font color="0000ff"><strong>#include <font color="#008000">&lt;stdlib.h&gt;</font></strong></font>
<font color="0000ff"><strong>#include <font color="#008000">&lt;string.h&gt;</font></strong></font>
<font color="0000ff"><strong>#include <font color="#008000">&lt;unistd.h&gt;</font></strong></font>

<font color="#444444">/*
 * char *realpath(const char *path, char resolved_path[MAXPATHLEN]);
 *
 * Find the real name of path, by removing all &quot;.&quot;, &quot;..&quot; and symlink
 * components.  Returns (resolved) on success, or (NULL) on failure,
 * in which case the path which caused trouble is left in (resolved).
 */</font>
<strong>char</strong> <font color="4444FF">*</font>
<font color="#2040a0">realpath</font><font color="4444FF">(</font><font color="#2040a0">path</font>, <font color="#2040a0">resolved</font><font color="4444FF">)</font>
	<strong>const</strong> <strong>char</strong> <font color="4444FF">*</font><font color="#2040a0">path</font><font color="4444FF">;</font>
	<strong>char</strong> <font color="4444FF">*</font><font color="#2040a0">resolved</font><font color="4444FF">;</font>
<font color="4444FF"><strong>{</strong></font>
	<strong>struct</strong> <font color="#2040a0">stat</font> <font color="#2040a0">sb</font><font color="4444FF">;</font>
	<strong>int</strong> <font color="#2040a0">fd</font>, <font color="#2040a0">n</font>, <font color="#2040a0">rootd</font>, <font color="#2040a0">serrno</font><font color="4444FF">;</font>
	<strong>char</strong> <font color="4444FF">*</font><font color="#2040a0">p</font>, <font color="4444FF">*</font><font color="#2040a0">q</font>, <font color="#2040a0">wbuf</font><font color="4444FF">[</font><font color="#2040a0">MAXPATHLEN</font><font color="4444FF">]</font><font color="4444FF">;</font>
      <strong>int</strong> <font color="#2040a0">symlinks</font> <font color="4444FF">=</font> <font color="#FF0000">0</font><font color="4444FF">;</font>

	<font color="#444444">/* Save the starting point. */</font>
	<strong>if</strong> <font color="4444FF">(</font><font color="4444FF">(</font><font color="#2040a0">fd</font> <font color="4444FF">=</font> <font color="#2040a0">open</font><font color="4444FF">(</font><font color="#008000">&quot;.&quot;</font>, <font color="#2040a0">O_RDONLY</font><font color="4444FF">)</font><font color="4444FF">)</font> <font color="4444FF">&lt;</font> <font color="#FF0000">0</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
		<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">strcpy</font><font color="4444FF">(</font><font color="#2040a0">resolved</font>, <font color="#008000">&quot;.&quot;</font><font color="4444FF">)</font><font color="4444FF">;</font>
		<strong>return</strong> <font color="4444FF">(</font><font color="#2040a0">NULL</font><font color="4444FF">)</font><font color="4444FF">;</font>
	<font color="4444FF"><strong>}</strong></font>

	<font color="#444444">/*
	 * Find the dirname and basename from the path to be resolved.
	 * Change directory to the dirname component.
	 * lstat the basename part.
	 *     if it is a symlink, read in the value and loop.
	 *     if it is a directory, then change to that directory.
	 * get the current directory name and append the basename.
	 */</font>
	<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">strncpy</font><font color="4444FF">(</font><font color="#2040a0">resolved</font>, <font color="#2040a0">path</font>, <font color="#2040a0">MAXPATHLEN</font> <font color="4444FF">-</font> <font color="#FF0000">1</font><font color="4444FF">)</font><font color="4444FF">;</font>
	<font color="#2040a0">resolved</font><font color="4444FF">[</font><font color="#2040a0">MAXPATHLEN</font> <font color="4444FF">-</font> <font color="#FF0000">1</font><font color="4444FF">]</font> <font color="4444FF">=</font> <font color="#008000">'<font color="#77dd77">\0</font>'</font><font color="4444FF">;</font>
<font color="#2040a0">loop</font><font color="4444FF">:</font>
	<font color="#2040a0">q</font> <font color="4444FF">=</font> <font color="#2040a0">strrchr</font><font color="4444FF">(</font><font color="#2040a0">resolved</font>, <font color="#008000">'/'</font><font color="4444FF">)</font><font color="4444FF">;</font>
	<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">q</font> <font color="4444FF">!</font><font color="4444FF">=</font> <font color="#2040a0">NULL</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
		<font color="#2040a0">p</font> <font color="4444FF">=</font> <font color="#2040a0">q</font> <font color="4444FF">+</font> <font color="#FF0000">1</font><font color="4444FF">;</font>
		<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">q</font> <font color="4444FF">=</font><font color="4444FF">=</font> <font color="#2040a0">resolved</font><font color="4444FF">)</font>
			<font color="#2040a0">q</font> <font color="4444FF">=</font> <font color="#008000">&quot;/&quot;</font><font color="4444FF">;</font>
		<strong>else</strong> <font color="4444FF"><strong>{</strong></font>
			<strong>do</strong> <font color="4444FF"><strong>{</strong></font>
				<font color="4444FF">-</font><font color="4444FF">-</font><font color="#2040a0">q</font><font color="4444FF">;</font>
			<font color="4444FF"><strong>}</strong></font> <strong>while</strong> <font color="4444FF">(</font><font color="#2040a0">q</font> <font color="4444FF">&gt;</font> <font color="#2040a0">resolved</font> <font color="4444FF">&amp;</font><font color="4444FF">&amp;</font> <font color="4444FF">*</font><font color="#2040a0">q</font> <font color="4444FF">=</font><font color="4444FF">=</font> <font color="#008000">'/'</font><font color="4444FF">)</font><font color="4444FF">;</font>
			<font color="#2040a0">q</font><font color="4444FF">[</font><font color="#FF0000">1</font><font color="4444FF">]</font> <font color="4444FF">=</font> <font color="#008000">'<font color="#77dd77">\0</font>'</font><font color="4444FF">;</font>
			<font color="#2040a0">q</font> <font color="4444FF">=</font> <font color="#2040a0">resolved</font><font color="4444FF">;</font>
		<font color="4444FF"><strong>}</strong></font>
		<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">chdir</font><font color="4444FF">(</font><font color="#2040a0">q</font><font color="4444FF">)</font> <font color="4444FF">&lt;</font> <font color="#FF0000">0</font><font color="4444FF">)</font>
			<strong>goto</strong> <font color="#2040a0">err1</font><font color="4444FF">;</font>
	<font color="4444FF"><strong>}</strong></font> <strong>else</strong>
		<font color="#2040a0">p</font> <font color="4444FF">=</font> <font color="#2040a0">resolved</font><font color="4444FF">;</font>

	<font color="#444444">/* Deal with the last component. */</font>
	<strong>if</strong> <font color="4444FF">(</font><font color="4444FF">*</font><font color="#2040a0">p</font> <font color="4444FF">!</font><font color="4444FF">=</font> <font color="#008000">'<font color="#77dd77">\0</font>'</font> <font color="4444FF">&amp;</font><font color="4444FF">&amp;</font> <font color="#2040a0">lstat</font><font color="4444FF">(</font><font color="#2040a0">p</font>, <font color="4444FF">&amp;</font><font color="#2040a0">sb</font><font color="4444FF">)</font> <font color="4444FF">=</font><font color="4444FF">=</font> <font color="#FF0000">0</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
		<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">S_ISLNK</font><font color="4444FF">(</font><font color="#2040a0">sb</font>.<font color="#2040a0">st_mode</font><font color="4444FF">)</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
                      <strong>if</strong> <font color="4444FF">(</font><font color="4444FF">+</font><font color="4444FF">+</font><font color="#2040a0">symlinks</font> <font color="4444FF">&gt;</font> <font color="#2040a0">MAXSYMLINKS</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
                              <font color="#2040a0">errno</font> <font color="4444FF">=</font> <font color="#2040a0">ELOOP</font><font color="4444FF">;</font>
                              <strong>goto</strong> <font color="#2040a0">err1</font><font color="4444FF">;</font>
                      <font color="4444FF"><strong>}</strong></font>
			<font color="#2040a0">n</font> <font color="4444FF">=</font> <font color="#2040a0">readlink</font><font color="4444FF">(</font><font color="#2040a0">p</font>, <font color="#2040a0">resolved</font>, <font color="#2040a0">MAXPATHLEN</font> <font color="4444FF">-</font> <font color="#FF0000">1</font><font color="4444FF">)</font><font color="4444FF">;</font>
			<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">n</font> <font color="4444FF">&lt;</font> <font color="#FF0000">0</font><font color="4444FF">)</font>
				<strong>goto</strong> <font color="#2040a0">err1</font><font color="4444FF">;</font>
			<font color="#2040a0">resolved</font><font color="4444FF">[</font><font color="#2040a0">n</font><font color="4444FF">]</font> <font color="4444FF">=</font> <font color="#008000">'<font color="#77dd77">\0</font>'</font><font color="4444FF">;</font>
			<strong>goto</strong> <font color="#2040a0">loop</font><font color="4444FF">;</font>
		<font color="4444FF"><strong>}</strong></font>
		<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">S_ISDIR</font><font color="4444FF">(</font><font color="#2040a0">sb</font>.<font color="#2040a0">st_mode</font><font color="4444FF">)</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
			<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">chdir</font><font color="4444FF">(</font><font color="#2040a0">p</font><font color="4444FF">)</font> <font color="4444FF">&lt;</font> <font color="#FF0000">0</font><font color="4444FF">)</font>
				<strong>goto</strong> <font color="#2040a0">err1</font><font color="4444FF">;</font>
			<font color="#2040a0">p</font> <font color="4444FF">=</font> <font color="#008000">&quot;&quot;</font><font color="4444FF">;</font>
		<font color="4444FF"><strong>}</strong></font>
	<font color="4444FF"><strong>}</strong></font>

	<font color="#444444">/*
	 * Save the last component name and get the full pathname of
	 * the current directory.
	 */</font>
	<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">strcpy</font><font color="4444FF">(</font><font color="#2040a0">wbuf</font>, <font color="#2040a0">p</font><font color="4444FF">)</font><font color="4444FF">;</font>
	<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">getcwd</font><font color="4444FF">(</font><font color="#2040a0">resolved</font>, <font color="#2040a0">MAXPATHLEN</font><font color="4444FF">)</font> <font color="4444FF">=</font><font color="4444FF">=</font> <font color="#FF0000">0</font><font color="4444FF">)</font>
		<strong>goto</strong> <font color="#2040a0">err1</font><font color="4444FF">;</font>

	<font color="#444444">/*
	 * Join the two strings together, ensuring that the right thing
	 * happens if the last component is empty, or the dirname is root.
	 */</font>
	<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">resolved</font><font color="4444FF">[</font><font color="#FF0000">0</font><font color="4444FF">]</font> <font color="4444FF">=</font><font color="4444FF">=</font> <font color="#008000">'/'</font> <font color="4444FF">&amp;</font><font color="4444FF">&amp;</font> <font color="#2040a0">resolved</font><font color="4444FF">[</font><font color="#FF0000">1</font><font color="4444FF">]</font> <font color="4444FF">=</font><font color="4444FF">=</font> <font color="#008000">'<font color="#77dd77">\0</font>'</font><font color="4444FF">)</font>
		<font color="#2040a0">rootd</font> <font color="4444FF">=</font> <font color="#FF0000">1</font><font color="4444FF">;</font>
	<strong>else</strong>
		<font color="#2040a0">rootd</font> <font color="4444FF">=</font> <font color="#FF0000">0</font><font color="4444FF">;</font>

	<strong>if</strong> <font color="4444FF">(</font><font color="4444FF">*</font><font color="#2040a0">wbuf</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
		<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">strlen</font><font color="4444FF">(</font><font color="#2040a0">resolved</font><font color="4444FF">)</font> <font color="4444FF">+</font> <font color="#2040a0">strlen</font><font color="4444FF">(</font><font color="#2040a0">wbuf</font><font color="4444FF">)</font> <font color="4444FF">+</font> <font color="4444FF">(</font><font color="#FF0000">1</font><font color="4444FF">-</font><font color="#2040a0">rootd</font><font color="4444FF">)</font> <font color="4444FF">+</font> <font color="#FF0000">1</font> <font color="4444FF">&gt;</font>
		    <font color="#2040a0">MAXPATHLEN</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
			<font color="#2040a0">errno</font> <font color="4444FF">=</font> <font color="#2040a0">ENAMETOOLONG</font><font color="4444FF">;</font>
			<strong>goto</strong> <font color="#2040a0">err1</font><font color="4444FF">;</font>
		<font color="4444FF"><strong>}</strong></font>
		<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">rootd</font> <font color="4444FF">=</font><font color="4444FF">=</font> <font color="#FF0000">0</font><font color="4444FF">)</font>
			<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">strcat</font><font color="4444FF">(</font><font color="#2040a0">resolved</font>, <font color="#008000">&quot;/&quot;</font><font color="4444FF">)</font><font color="4444FF">;</font>
		<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">strcat</font><font color="4444FF">(</font><font color="#2040a0">resolved</font>, <font color="#2040a0">wbuf</font><font color="4444FF">)</font><font color="4444FF">;</font>
	<font color="4444FF"><strong>}</strong></font>

	<font color="#444444">/* Go back to where we came from. */</font>
	<strong>if</strong> <font color="4444FF">(</font><font color="#2040a0">fchdir</font><font color="4444FF">(</font><font color="#2040a0">fd</font><font color="4444FF">)</font> <font color="4444FF">&lt;</font> <font color="#FF0000">0</font><font color="4444FF">)</font> <font color="4444FF"><strong>{</strong></font>
		<font color="#2040a0">serrno</font> <font color="4444FF">=</font> <font color="#2040a0">errno</font><font color="4444FF">;</font>
		<strong>goto</strong> <font color="#2040a0">err2</font><font color="4444FF">;</font>
	<font color="4444FF"><strong>}</strong></font>

	<font color="#444444">/* It's okay if the close fails, what's an fd more or less? */</font>
	<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">close</font><font color="4444FF">(</font><font color="#2040a0">fd</font><font color="4444FF">)</font><font color="4444FF">;</font>
	<strong>return</strong> <font color="4444FF">(</font><font color="#2040a0">resolved</font><font color="4444FF">)</font><font color="4444FF">;</font>

<font color="#2040a0">err1</font><font color="4444FF">:</font>	<font color="#2040a0">serrno</font> <font color="4444FF">=</font> <font color="#2040a0">errno</font><font color="4444FF">;</font>
	<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">fchdir</font><font color="4444FF">(</font><font color="#2040a0">fd</font><font color="4444FF">)</font><font color="4444FF">;</font>
<font color="#2040a0">err2</font><font color="4444FF">:</font>	<font color="4444FF">(</font><strong>void</strong><font color="4444FF">)</font><font color="#2040a0">close</font><font color="4444FF">(</font><font color="#2040a0">fd</font><font color="4444FF">)</font><font color="4444FF">;</font>
	<font color="#2040a0">errno</font> <font color="4444FF">=</font> <font color="#2040a0">serrno</font><font color="4444FF">;</font>
	<strong>return</strong> <font color="4444FF">(</font><font color="#2040a0">NULL</font><font color="4444FF">)</font><font color="4444FF">;</font>
<font color="4444FF"><strong>}</strong></font>

</pre>
<hr>
syntax highlighted by <a href="http://www.palfrader.org/code2html">Code2HTML</a>, v. 0.9.1
</body>
</html>
