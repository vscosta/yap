Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.26.2
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2262.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2262
%build
%install
rm -rf /usr/local/lgt2262
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2262
cp -R * /usr/local/lgt2262
cd /usr/local
chmod -R go-w,a+r lgt2262
chmod a+x lgt2262
chmod a+x lgt2262/scripts/*.sh
chmod a+x lgt2262/xml/*.sh
ln -sf lgt2262 logtalk
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2262/BIBLIOGRAPHY
%doc /usr/local/lgt2262/INSTALL
%doc /usr/local/lgt2262/LICENSE
%doc /usr/local/lgt2262/QUICK_START
%doc /usr/local/lgt2262/README
%doc /usr/local/lgt2262/RELEASE_NOTES
%doc /usr/local/lgt2262/UPGRADING
/usr/local/lgt2262/compiler
/usr/local/lgt2262/configs
/usr/local/lgt2262/contributions
/usr/local/lgt2262/examples
/usr/local/lgt2262/libpaths
/usr/local/lgt2262/library
%docdir /usr/local/lgt2262/manuals
/usr/local/lgt2262/manuals
/usr/local/lgt2262/scripts
/usr/local/lgt2262/wenv
/usr/local/lgt2262/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
