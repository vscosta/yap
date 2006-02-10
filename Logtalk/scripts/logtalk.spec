Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.27.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2270.tgz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2270
%build
%install
rm -rf /usr/local/lgt2270
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2270
cp -R * /usr/local/lgt2270
cd /usr/local
chmod -R go-w,a+r lgt2270
chmod a+x lgt2270
chmod a+x lgt2270/scripts/*.sh
chmod a+x lgt2270/xml/*.sh
ln -sf lgt2270 logtalk
cd bin
ln -sf ../logtalk/scripts/cplgtdirs.sh cplgtdirs
ln -sf ../logtalk/xml/lgt2pdf.sh lgt2pdf
ln -sf ../logtalk/xml/lgt2html.sh lgt2html
ln -sf ../logtalk/xml/lgt2xml.sh lgt2xml
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2270/BIBLIOGRAPHY
%doc /usr/local/lgt2270/INSTALL
%doc /usr/local/lgt2270/LICENSE
%doc /usr/local/lgt2270/QUICK_START
%doc /usr/local/lgt2270/README
%doc /usr/local/lgt2270/RELEASE_NOTES
%doc /usr/local/lgt2270/UPGRADING
/usr/local/lgt2270/compiler
/usr/local/lgt2270/configs
/usr/local/lgt2270/contributions
/usr/local/lgt2270/examples
/usr/local/lgt2270/libpaths
/usr/local/lgt2270/library
%docdir /usr/local/lgt2270/manuals
/usr/local/lgt2270/manuals
/usr/local/lgt2270/scripts
/usr/local/lgt2270/wenv
/usr/local/lgt2270/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs
/usr/local/bin/lgt2pdf
/usr/local/bin/lgt2html
/usr/local/bin/lgt2xml
