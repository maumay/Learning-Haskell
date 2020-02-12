package com.github.maumay.tacocloud

import org.hibernate.validator.constraints.CreditCardNumber
import java.util.*
import javax.validation.constraints.Digits
import javax.validation.constraints.NotBlank
import javax.validation.constraints.Pattern

class Order {
    var id: Long? = null
    var placedAt: Date? = null

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

interface OrderRepository {
    fun save(order: Order): Order
}