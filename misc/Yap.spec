Name: Yap
Summary: Prolog Compiler
Version: 4.5.1
Packager:     Vitor Santos Costa <vitor@cos.ufrj.br>
Release: 1
Source: http://www.ncc.up.pt/~vsc/Yap/%{name}-%{version}.tar.gz
Copyright: Perl Artistic License
Provides: yap
Requires: readline
Group: Development/Languages
URL: http://www.ncc.up.pt/~vsc/Yap
Prefix: /usr

%description
A high-performance Prolog compiler developed at LIACC,
Universidade do Porto. The Prolog engine is based in the WAM (Warren
Abstract Machine), with several optimizations for better
performance. YAP follows the Edinburgh tradition, and is largely
compatible with the ISO-Prolog standard and with Quintus and SICStus
Prolog.

%prep
rm -rf $RPM_BUILD_ROOT

%setup -q

%build
./configure --prefix=/usr  --enable-coroutining --enable-max-performance
make

%install
make install
make install_info

%post
/sbin/install-info  --quiet /usr/share/info/yap.info --section "Programming Languages" /usr/share/info/dir
/sbin/install-info  --quiet /usr/share/info/pillow_doc.info --section "Programming Languages" /usr/share/info/dir

%postun
/sbin/install-info  --quiet --delete yap.info /usr/share/info/dir
/sbin/install-info  --quiet --delete pillow_doc.info /usr/share/info/dir

rm -f /usr/info/yap.info*

%clean
rm -rf $RPM_BUILD_ROOT $RPM_BUILD_DIR/file.list.%{name}

%files
%doc README*
%doc INSTALL
%doc changes4.3.html
%doc docs/yap.tex
/usr/bin/yap
/usr/lib/Yap/
/usr/share/info/yap.info*
/usr/share/info/pillow_doc.info*

%changelog

