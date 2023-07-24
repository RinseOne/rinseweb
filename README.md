# rinseweb

![Test](https://github.com/RinseOne/rinseweb/actions/workflows/test.yml/badge.svg)
![Release](https://github.com/RinseOne/rinseweb/actions/workflows/release.yml/badge.svg)
![Test](https://img.shields.io/github/actions/workflow/status/RinseOne/rinseweb/test)
![Release](https://img.shields.io/github/actions/workflow/status/RinseOne/rinseweb/release)
![GitHub](https://img.shields.io/github/license/RinseOne/rinseweb?style=for-the-badge)

Web application for providing answers using configured backends written in Erlang/OTP.

## Prerequisites

* GNU make
* Erlang/OTP
* [GNU units](https://www.gnu.org/software/units/) `units` executable in path

## Run

Run application locally
```
make run
```
then go to http://localhost:8080

## Tests

Run tests
```
make tests
```

Filter to selected test suites only using the `filter` argument, for example
```
make tests filter=test/util_SUITE,test/uuid_SUITE
```

Generate test coverage report
```
make cover
```

## Release

### Automatic

Pick a new version and run `make release`. For example, for version `1.2.3`:

```
make release version=1.2.3
```

### Manual

This does the same as the automatic method with manual commands.

Update version to prepare for release; for example, to set version `1.2.3`, tag it, and commit and push the change:
```
make set-version version=1.2.3
git commit -a -m "Update version to 1.2.3"
git tag 1.2.3
git push --atomic origin main 1.2.3
```
