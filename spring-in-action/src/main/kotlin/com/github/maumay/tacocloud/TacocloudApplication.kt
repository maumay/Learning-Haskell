package com.github.maumay.tacocloud

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class TacocloudApplication

fun main(args: Array<String>) {
	runApplication<TacocloudApplication>(*args)
}
