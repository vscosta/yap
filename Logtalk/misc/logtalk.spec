Name: logtalk
Summary: Logtalk - Open source object-oriented extension to Prolog
Version: 2.19.0
Release: 1
License: Artistic License 2.0
Group: Development/Languages
Packager: Paulo Moura <pmoura@logtalk.org>
Source: http://www.logtalk.org/files/lgt2190.tar.gz
BuildArchitectures: noarch
URL: http://www.logtalk.org/
Prefix: /usr/local
AutoReqProv: no
%description
Logtalk is an open source object-oriented extension to the Prolog programming language. Integrating logic programming with object-oriented and event-driven programming, it is compatible with most Prolog compilers. It supports both prototypes and classes. In addition, it supports component-based programming through category-based composition.
%prep
%setup -n lgt2190
%build
%install
rm -rf /usr/local/lgt2190
rm -f /usr/local/logtalk
mkdir /usr/local/lgt2190
cp -R * /usr/local/lgt2190
cd /usr/local
chmod -R go-w,a+r lgt2190
chmod a+x lgt2190
chmod a+x lgt2190/misc/*.sh
chmod a+x lgt2190/xml/*.sh
ln -sf lgt2190 logtalk
cd bin
ln -sf ../lgt2190/misc/cplgtdirs.sh cplgtdirs.sh
%clean
%files
%defattr(-,root,users)
%doc /usr/local/lgt2190/BIBLIOGRAPHY
%doc /usr/local/lgt2190/INSTALL
%doc /usr/local/lgt2190/LICENSE
%doc /usr/local/lgt2190/QUICK_START
%doc /usr/local/lgt2190/README
%doc /usr/local/lgt2190/RELEASE_NOTES
%doc /usr/local/lgt2190/UPGRADING
/usr/local/lgt2190/compiler
/usr/local/lgt2190/configs
/usr/local/lgt2190/examples
/usr/local/lgt2190/library
%docdir /usr/local/lgt2190/manuals
/usr/local/lgt2190/manuals
/usr/local/lgt2190/misc
/usr/local/lgt2190/wenv
/usr/local/lgt2190/xml
/usr/local/logtalk
/usr/local/bin/cplgtdirs.sh
