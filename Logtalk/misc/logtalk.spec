Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.22.4
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2224.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2224
%build
%install
rm -rf /usr/local/lgt2224
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2224
cp -R * /usr/local/lgt2224
cd /usr/local
chmod -R go-w,a+r lgt2224
chmod a+x lgt2224
chmod a+x lgt2224/misc/*.sh
chmod a+x lgt2224/xml/*.sh
ln -sf lgt2224 logtalk
cd bin
ln -sf ../lgt2224/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2224/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2224/xml/lgt2html.sh lgt2html
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2224/BIBLIOGRAPHY
%doc /usr/local/lgt2224/INSTALL
%doc /usr/local/lgt2224/LICENSE
%doc /usr/local/lgt2224/QUICK_START
%doc /usr/local/lgt2224/README
%doc /usr/local/lgt2224/RELEASE_NOTES
%doc /usr/local/lgt2224/UPGRADING
/usr/local/lgt2224/compiler
/usr/local/lgt2224/configs
/usr/local/lgt2224/examples
/usr/local/lgt2224/libpaths
/usr/local/lgt2224/library
%docdir /usr/local/lgt2224/manuals
/usr/local/lgt2224/manuals
/usr/local/lgt2224/misc
/usr/local/lgt2224/wenv
/usr/local/lgt2224/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
