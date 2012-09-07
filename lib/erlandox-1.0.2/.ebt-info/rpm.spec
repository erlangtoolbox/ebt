Name: erlandox
Release: 0%{?dist}
Version: 1.0.2
Summary: ErlandoX
License: Erlang Public License
Group: Development
BuildArch: noarch
Requires: erlang = R15B01-1.1

%description
ErlandoX

%define _erlang_lib /usr/%{_lib}/erlang/lib/erlandox
%define _target %{buildroot}%{_erlang_lib}

%install
mkdir -p %{_target}
for f in %{_builddir}/*
do
    cp -r $f %{_target}
done

if [ -d  %{_builddir}/bin ]
then
    mkdir -p %{buildroot}%{_bindir}
    for f in %{_builddir}/bin/*
    do
        FILE=`basename $f`
        ln -s %{_erlang_lib}/bin/$FILE %{buildroot}%{_bindir}/$FILE
    done
fi

if [ -d  %{_builddir}/etc ]
then
    mkdir -p %{buildroot}%{_sysconfdir}
    for f in %{_builddir}/etc/*
    do
        cp -r $f %{buildroot}%{_sysconfdir}
    done
fi

%files
/




%changelog
