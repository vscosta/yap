# Release Notes - 7.5.0

- Implementation of Iterator interface in `Query` to match Java Iterator standard.
    - Pointer to next solution is advanced only via `next()` and `nextSolution()`, but not on `hasNext()` or `hasMoreSolutions()`.
- Deprecated `Query.getSolution()`
- Revert complex quoting of atoms for `Atom` and `Compound`.
- Added standalong JUnit test cases in `src/java/test/standalone`

## Internal

- Removed reference to deprecated methods that yielded warnings at compile time.
- Renamed `Query.get1()` and `Query.get2()`


## Versions up to 7.4

- Complete overhawl from version 3.x
- Check release notes for [7.4.0](https://jpl7.org/ReleaseNotes740.jsp).
- Check release notes for [7.0.1](https://jpl7.org/ReleaseNotes701.jsp).



# Versions 3.x

- Check [here](RELEASE-NOTES-3.md).
