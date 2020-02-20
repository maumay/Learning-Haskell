package com.github.maumay.tacocloud

import org.slf4j.Logger
import org.slf4j.LoggerFactory
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder
import org.springframework.security.config.annotation.web.builders.HttpSecurity
import org.springframework.security.config.annotation.web.builders.WebSecurity
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter
import org.springframework.security.core.userdetails.UserDetailsService
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder
import org.springframework.security.crypto.password.PasswordEncoder
import org.springframework.security.crypto.password.StandardPasswordEncoder
import org.springframework.stereotype.Controller
import org.springframework.ui.Model
import org.springframework.validation.Errors
import org.springframework.web.bind.annotation.*
import org.springframework.web.bind.support.SessionStatus
import org.springframework.web.servlet.config.annotation.ViewControllerRegistry
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer
import javax.sql.DataSource
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

//inline fun <T : Enum<T>> KClass<T>.values(): List<T> {
//	return List(java.enumConstants.size) {java.enumConstants[it]}
//}

@Controller
@RequestMapping("/design")
@SessionAttributes("order")
class DesignTacoController @Autowired constructor(
        private val ingredientRepo: IngredientRepository,
        private val tacoRepository: TacoRepository
) {
	companion object {
		private val log: Logger = LoggerFactory.getLogger(DesignTacoController::class.java)
	}

	@ModelAttribute(name = "order")
	fun order(): Order = Order()

	@ModelAttribute(name = "taco")
	fun taco(): Taco = Taco()

	@GetMapping
	fun designGet(model: Model): String {
		val ingredients = ingredientRepo.findAll().toList()
		for (type in IngredientType.values()) {
			model.addAttribute(type.name.toLowerCase(), ingredients.filter { it.type == type })
		}
		return "design"
	}

	@PostMapping
	fun processDesign(model: Model, @Valid design: Taco, errors: Errors, @ModelAttribute(binding = false) order: Order): String {
		if (errors.hasErrors()) {
			return designGet(model)
		}
        tacoRepository.save(design)
		order.tacos.add(design)
		log.info(design.toString())
		return "redirect:/orders/current"
	}
}

@Controller
@RequestMapping("/orders")
@SessionAttributes("order")
class OrderController(@Autowired private val orderRepository: OrderRepository) {
	companion object {
		private val log: Logger = LoggerFactory.getLogger(OrderController::class.java)
	}

	@GetMapping("/current")
	fun orderForm(model: Model): String {
		model.addAttribute("order", Order())
		return "orderForm"
	}

    @PostMapping
	fun processOrder(@Valid order: Order, errors: Errors, sessionStatus: SessionStatus): String {
		if (errors.hasErrors())
			return "orderForm"
		log.info("Saved $order")
		orderRepository.save(order)
		sessionStatus.setComplete()
		return "redirect:/"
	}
}

@Configuration
@EnableWebSecurity
class SecurityConfig(@Autowired private val userDetailsService: UserDetailsService) : WebSecurityConfigurerAdapter() {
	@Bean
    fun encoder(): PasswordEncoder = BCryptPasswordEncoder()

	override fun configure(auth: AuthenticationManagerBuilder?) {
		auth?.userDetailsService(userDetailsService)?.passwordEncoder(encoder())
	}

	override fun configure(web: WebSecurity?) {
		web!!.ignoring().antMatchers("/h2-console/**")
	}

	override fun configure(http: HttpSecurity?) {
		http!!.authorizeRequests()
				.antMatchers("/design", "/orders").hasRole("USER")
				.antMatchers("/", "/**").permitAll()
		super.configure(http)
	}
}

@Controller
@RequestMapping("/register")
class RegistrationController @Autowired constructor(
		private val userRepository: UserRepository,
		private val passwordEncoder: PasswordEncoder) {

	@GetMapping
	fun registerForm() = "registration"

	@ModelAttribute(name = "registerForm")
	fun registerFormAttribute() = RegistrationForm()

	@PostMapping
	fun processRegistration(form: RegistrationForm): String {
		userRepository.save(form.toUser(passwordEncoder))
		return "redirect:/login"
	}
}