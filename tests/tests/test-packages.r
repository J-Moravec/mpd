TEST_SET("packages.r", {
    pkgs = c("base", "mpd")
    invalid = "not even valid name"

    # lib.loc needs to be explicitly specified for some reason
    expected = setNames(find.package(pkgs, lib.loc = .libPaths()), pkgs)
    TEST(identical(find_packages(pkgs), expected))
    TEST(is.na(find_packages(invalid)))
    TEST(all(installed_packages(pkgs)))
    TEST(!installed_packages(invalid))

    expected = list("mpd" = c("tools", "utils"))
    TEST(identical(package_dependencies("mpd"), expected))
    })
