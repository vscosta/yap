Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.22.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2220.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2220
%build
%install
rm -rf /usr/local/lgt2220
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2220
cp -R * /usr/local/lgt2220
cd /usr/local
chmod -R go-w,a+r lgt2220
chmod a+x lgt2220
chmod a+x lgt2220/misc/*.sh
chmod a+x lgt2220/xml/*.sh
ln -sf lgt2220 logtalk
cd bin
ln -sf ../lgt2220/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2220/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2220/xml/lgt2html.sh lgt2html
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2220/BIBLIOGRAPHY
%doc /usr/local/lgt2220/INSTALL
%doc /usr/local/lgt2220/LICENSE
%doc /usr/local/lgt2220/QUICK_START
%doc /usr/local/lgt2220/README
%doc /usr/local/lgt2220/RELEASE_NOTES
%doc /usr/local/lgt2220/UPGRADING
/usr/local/lgt2220/compiler
/usr/local/lgt2220/configs
/usr/local/lgt2220/examples
/usr/local/lgt2220/libpaths
/usr/local/lgt2220/library
%docdir /usr/local/lgt2220/manuals
/usr/local/lgt2220/manuals
/usr/local/lgt2220/misc
/usr/local/lgt2220/wenv
/usr/local/lgt2220/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
