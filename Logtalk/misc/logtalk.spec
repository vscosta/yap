Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.21.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2210.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2210
%build
%install
rm -rf /usr/local/lgt2210
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2210
cp -R * /usr/local/lgt2210
cd /usr/local
chmod -R go-w,a+r lgt2210
chmod a+x lgt2210
chmod a+x lgt2210/misc/*.sh
chmod a+x lgt2210/xml/*.sh
ln -sf lgt2210 logtalk
cd bin
ln -sf ../lgt2210/misc/cplgtdirs.sh cplgtdirs
ln -sf ../lgt2210/xml/lgt2pdf.sh lgt2pdf
ln -sf ../lgt2210/xml/lgt2html.sh lgt2html
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2210/BIBLIOGRAPHY
%doc /usr/local/lgt2210/INSTALL
%doc /usr/local/lgt2210/LICENSE
%doc /usr/local/lgt2210/QUICK_START
%doc /usr/local/lgt2210/README
%doc /usr/local/lgt2210/RELEASE_NOTES
%doc /usr/local/lgt2210/UPGRADING
/usr/local/lgt2210/compiler
/usr/local/lgt2210/configs
/usr/local/lgt2210/examples
/usr/local/lgt2210/library
%docdir /usr/local/lgt2210/manuals
/usr/local/lgt2210/manuals
/usr/local/lgt2210/misc
/usr/local/lgt2210/wenv
/usr/local/lgt2210/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
