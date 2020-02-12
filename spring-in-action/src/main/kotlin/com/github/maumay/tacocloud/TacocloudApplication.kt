package com.github.maumay.tacocloud

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.validation.Errors
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.PostMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.SessionAttributes
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer
import javax.validation.Valid
import kotlin.reflect.KClass

@SpringBootApplication
class TacocloudApplication : WebMvcConfigurer {
	override fun addViewControllers(registry: ViewControllerRegistry) {
		registry.addViewController("/").setViewName("home")
	}
}

fun main(args: Array<String>) {
	runApplication<TacocloudApplication>(*args)
}

inline fun <T : Enum<T>> KClass<T>.values(): List<T> {
	return List(java.enumConstants.size) {java.enumConstants[it]}
}

@Controller
@RequestMapping("/design")
@SessionAttributes("order")
class DesignTacoController(@Autowired private val ingredientRepo: IngredientRepository) {
	companion object {
		private val log: Logger = LoggerFactory.getLogger(DesignTacoController::class.java)
	}

	@GetMapping
	fun designGet(model: Model): String {
		val ingredients = ingredientRepo.findAll().toList()
		for (type in IngredientType.values()) {
			model.addAttribute(type.name.toLowerCase(), ingredients.filter { it.type == type })
		}
		model.addAttribute("taco", Taco())
		return "design"
	}

	@PostMapping
	fun processDesign(model: Model, @Valid design: Taco, errors: Errors): String {
		if (errors.hasErrors()) {
			return designGet(model)
		}
		log.info(design.toString())
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
