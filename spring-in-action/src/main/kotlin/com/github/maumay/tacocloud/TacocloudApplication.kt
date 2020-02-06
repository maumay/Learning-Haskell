package com.github.maumay.tacocloud

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.stereotype.Controller
import org.springframework.web.bind.annotation.GetMapping
import java.lang.ClassCastException
import java.util.*
import kotlin.reflect.jvm.internal.impl.load.kotlin.JvmType

@SpringBootApplication
class TacocloudApplication

fun main(args: Array<String>) {
	runApplication<TacocloudApplication>(*args)
}

@Controller
class HomeController {
	@GetMapping("/")
	fun home(): String {
		return "home"
	}
}
