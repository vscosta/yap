Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.22.2
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2222.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2222
%build
%install
rm -rf /usr/local/lgt2222
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2222
cp -R * /usr/local/lgt2222
cd /usr/local
chmod -R go-w,a+r lgt2222
chmod a+x lgt2222
chmod a+x lgt2222/misc/*.sh
chmod a+x lgt2222/xml/*.sh
ln -sf lgt2222 logtalk
cd bin
ln -sf ../lgt2222/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2222/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2222/xml/lgt2html.sh lgt2html
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2222/BIBLIOGRAPHY
%doc /usr/local/lgt2222/INSTALL
%doc /usr/local/lgt2222/LICENSE
%doc /usr/local/lgt2222/QUICK_START
%doc /usr/local/lgt2222/README
%doc /usr/local/lgt2222/RELEASE_NOTES
%doc /usr/local/lgt2222/UPGRADING
/usr/local/lgt2222/compiler
/usr/local/lgt2222/configs
/usr/local/lgt2222/examples
/usr/local/lgt2222/libpaths
/usr/local/lgt2222/library
%docdir /usr/local/lgt2222/manuals
/usr/local/lgt2222/manuals
/usr/local/lgt2222/misc
/usr/local/lgt2222/wenv
/usr/local/lgt2222/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
