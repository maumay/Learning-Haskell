package com.github.maumay.tacocloud

import org.hamcrest.Matchers.containsString
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*
import org.springframework.test.web.servlet.result.MockMvcResultMatchers.*

import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.web.servlet.MockMvc

@SpringBootTest
class TacocloudApplicationTests {
	@Test
	fun contextLoads() {
	}
}

@WebMvcTest(HomeController::class)
class HomeControllerTest {
	@Autowired
	private var mockMvc: MockMvc? = null

	@Test
	fun testHomePage() {
		mockMvc!!.perform(get("/"))
				.andExpect(status().isOk)
				.andExpect(view().name("home"))
				.andExpect(content().string(containsString("Welcome to...")))
	}
}
