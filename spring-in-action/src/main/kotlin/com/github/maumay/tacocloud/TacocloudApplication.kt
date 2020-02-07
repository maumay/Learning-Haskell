package com.github.maumay.tacocloud

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.validation.Errors
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestMapping
import java.lang.ClassCastException
import java.util.*
import javax.validation.Valid
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
	companion object {
		private val log: Logger = LoggerFactory.getLogger(DesignTacoController::class.java)
	}

	@GetMapping
	fun designGet(model: Model): String {
		println("hi $model")
		for (type in Type.values()) {
			model.addAttribute(type.name.toLowerCase(), ingredients.filter { it.type == type })
		}
		model.addAttribute("design", Taco())
		return "design"
	}

	@PostMapping
	fun processDesign(model: Model, @Valid design: Taco, errors: Errors): String {
		println("hi $model")
		if (errors.hasErrors()) {
            model.addAttribute("something", "x")
			return "redirect:/design"
		}

		log.info(design.toString())
		log.info(model.toString())
		return "redirect:/orders/current"
	}
}

@Controller
@RequestMapping("/orders")
class OrderController {
	companion object {
		private val log: Logger = LoggerFactory.getLogger(OrderController::class.java)
	}

	@GetMapping("/current")
	fun orderForm(model: Model): String {
		model.addAttribute("order", Order())
		return "orderForm"
	}

    @PostMapping
	fun processOrder(@Valid order: Order, model: Model, errors: Errors): String {
		if (errors.hasErrors())
			return "orderForm"
		log.info(order.toString())
		log.info(model.toString())
		return "redirect:/"
	}
}
