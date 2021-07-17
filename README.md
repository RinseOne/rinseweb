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

## Convenience

Update version to prepare for release; for example, to set version `1.2.3`:
```
make set-version version=1.2.3
```
