Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.21.3
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2213.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2213
%build
%install
rm -rf /usr/local/lgt2213
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2213
cp -R * /usr/local/lgt2213
cd /usr/local
chmod -R go-w,a+r lgt2213
chmod a+x lgt2213
chmod a+x lgt2213/misc/*.sh
chmod a+x lgt2213/xml/*.sh
ln -sf lgt2213 logtalk
cd bin
ln -sf ../lgt2213/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2213/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2213/xml/lgt2html.sh lgt2html
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2213/BIBLIOGRAPHY
%doc /usr/local/lgt2213/INSTALL
%doc /usr/local/lgt2213/LICENSE
%doc /usr/local/lgt2213/QUICK_START
%doc /usr/local/lgt2213/README
%doc /usr/local/lgt2213/RELEASE_NOTES
%doc /usr/local/lgt2213/UPGRADING
/usr/local/lgt2213/compiler
/usr/local/lgt2213/configs
/usr/local/lgt2213/examples
/usr/local/lgt2213/library
%docdir /usr/local/lgt2213/manuals
/usr/local/lgt2213/manuals
/usr/local/lgt2213/misc
/usr/local/lgt2213/wenv
/usr/local/lgt2213/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
