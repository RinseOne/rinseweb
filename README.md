# rinseweb

![CircleCI](https://img.shields.io/circleci/build/github/RinseOne/rinseweb?style=for-the-badge)
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

Run tests
```
make tests
```

## Release

Update version to prepare for release; for example, to set version `1.2.3`:
```
make set-version version=1.2.3
```
and then commit and push the change.

Use `git tag` to tag and push a release. This will trigger to create a new release automatically via GitHub workflows. For example:
```
git tag 1.2.3
git push origin 1.2.3
```
