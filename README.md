## Introduction
This project is a copy of the [Spring-framework-3.2.3.RELEASE][] source code with Maven support, e.g., pom.xml, which means that you can import this project into your IDE using either Gradle or Maven.

## TODO
* spring-aspects
    Due to the compilation error, this module is omitted from the parent/root pom.xml.
* unit test
    Almost all the unit tests cannot be compiled correctly.

## Usage
`mvn clean compile package -Dmaven.test.skip=true`

[Spring-framework-3.2.3.RELEASE]: https://github.com/spring-projects/spring-framework/tree/v3.2.3.RELEASE

