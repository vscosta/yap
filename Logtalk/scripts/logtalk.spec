Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.25.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2250.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2250
%build
%install
rm -rf /usr/local/lgt2250
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2250
cp -R * /usr/local/lgt2250
cd /usr/local
chmod -R go-w,a+r lgt2250
chmod a+x lgt2250
chmod a+x lgt2250/scripts/*.sh
chmod a+x lgt2250/xml/*.sh
ln -sf lgt2250 logtalk
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2250/BIBLIOGRAPHY
%doc /usr/local/lgt2250/INSTALL
%doc /usr/local/lgt2250/LICENSE
%doc /usr/local/lgt2250/QUICK_START
%doc /usr/local/lgt2250/README
%doc /usr/local/lgt2250/RELEASE_NOTES
%doc /usr/local/lgt2250/UPGRADING
/usr/local/lgt2250/compiler
/usr/local/lgt2250/configs
/usr/local/lgt2250/contributions
/usr/local/lgt2250/examples
/usr/local/lgt2250/libpaths
/usr/local/lgt2250/library
%docdir /usr/local/lgt2250/manuals
/usr/local/lgt2250/manuals
/usr/local/lgt2250/scripts
/usr/local/lgt2250/wenv
/usr/local/lgt2250/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
