package com.github.maumay.tacocloud

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import java.lang.ClassCastException
import java.util.*
import kotlin.reflect.jvm.internal.impl.load.kotlin.JvmType

@SpringBootApplication
class TacocloudApplication

fun main(args: Array<String>) {
	runApplication<TacocloudApplication>(*args)
}

typealias Type = IngredientType
val ingredients = listOf(
		Ingredient("FLTO", "Flour Tortilla", Type.WRAP),
		Ingredient("COTO", "Corn Tortilla", Type.WRAP),
		Ingredient("GRBF", "Ground Beef", Type.PROTEIN),
		Ingredient("CARN", "Carnitas", Type.PROTEIN),
		Ingredient("TMTO", "Diced Tomatoes", Type.VEGGIES),
		Ingredient("LETC", "Lettuce", Type.VEGGIES),
		Ingredient("CHED", "Cheddar", Type.CHEESE),
		Ingredient("JACK", "Monterrey Jack", Type.CHEESE),
		Ingredient("SLSA", "Salsa", Type.SAUCE),
		Ingredient("SRCR", "Sour Cream", Type.SAUCE)
)

@Controller
class HomeController {
	@GetMapping(path= ["/"])
	fun home(model: Model): String {
		return "home"
	}
}

@Controller
@RequestMapping("/design")
class DesignTacoController {
	@GetMapping
	fun designGet(model: Model): String {
		for (type in Type.values()) {
			model.addAttribute(type.name.toLowerCase(), ingredients.filter { it.type == type })
		}
        println(model)
		return "design"
	}
}
