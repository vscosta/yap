Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.19.1
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2191.tar.gz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2191
%build
%install
rm -rf /usr/local/lgt2191
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2191
cp -R * /usr/local/lgt2191
cd /usr/local
chmod -R go-w,a+r lgt2191
chmod a+x lgt2191
chmod a+x lgt2191/misc/*.sh
chmod a+x lgt2191/xml/*.sh
ln -sf lgt2191 logtalk
cd bin
ln -sf ../lgt2191/misc/cplgtdirs.sh cplgtdirs.sh
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2191/BIBLIOGRAPHY
%doc /usr/local/lgt2191/INSTALL
%doc /usr/local/lgt2191/LICENSE
%doc /usr/local/lgt2191/QUICK_START
%doc /usr/local/lgt2191/README
%doc /usr/local/lgt2191/RELEASE_NOTES
%doc /usr/local/lgt2191/UPGRADING
/usr/local/lgt2191/compiler
/usr/local/lgt2191/configs
/usr/local/lgt2191/examples
/usr/local/lgt2191/library
%docdir /usr/local/lgt2191/manuals
/usr/local/lgt2191/manuals
/usr/local/lgt2191/misc
/usr/local/lgt2191/wenv
/usr/local/lgt2191/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs.sh
