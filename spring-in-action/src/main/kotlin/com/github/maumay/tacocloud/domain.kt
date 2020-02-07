package com.github.maumay.tacocloud

import org.hibernate.validator.constraints.CreditCardNumber
import javax.validation.constraints.*

enum class IngredientType { WRAP, PROTEIN, VEGGIES, CHEESE, SAUCE }
data class Ingredient(val id: String, val name: String, val type: IngredientType)

class Taco {
    @NotNull
    @Size(min=5, message="Name must be at least 5 characters long.")
    var name: String? = null

    @Size(min=1, message="Must be at least one ingredient.")
    var ingredients: List<String>? = null

    override fun toString(): String {
        return "Taco[name=$name, ingredients=$ingredients]"
    }
}

class Order {
    @NotBlank(message="Name required.")
    var name: String? = null

    @NotBlank(message="Street required.")
    var street: String? = null

    @NotBlank(message="City required.")
    var city: String? = null

    @NotBlank(message="State required.")
    var state: String? = null

    @NotBlank(message="Zip required.")
    var zip: String? = null

    @CreditCardNumber(message="Not a valid credit card number.")
    var ccNumber: String? = null

    @Pattern(regexp="^(0[1-9]|1[0-2])([\\/])([1-9][0-9])$", message="Must be formatted MM/YY")
    var ccExpiration: String? = null

    @Digits(integer = 3, fraction = 0, message = "Invalid CVV")
    var ccCVV: String? = null
}
