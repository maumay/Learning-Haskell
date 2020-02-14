package com.github.maumay.tacocloud

import com.fasterxml.jackson.databind.ObjectMapper
import org.hibernate.validator.constraints.CreditCardNumber
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.repository.CrudRepository
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.core.simple.SimpleJdbcInsert
import org.springframework.stereotype.Repository
import java.util.*
import javax.persistence.*
import javax.validation.constraints.Digits
import javax.validation.constraints.NotBlank
import javax.validation.constraints.Pattern

@Entity
@Table(name = "Taco_Order")
class Order {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    var id: Long? = null

    var placedAt: Date? = null

    @NotBlank(message="Name required.")
    var name: String? = null

    @NotBlank(message="Street required.")
    var street: String? = null

    @NotBlank(message="City required.")
    var city: String? = null

    @IsState(message = "My custom message")
    var state: String? = null

    @NotBlank(message="Zip required.")
    var zip: String? = null

    @CreditCardNumber(message="Not a valid credit card number.")
    var ccNumber: String? = null

    @Pattern(regexp="^(0[1-9]|1[0-2])([\\/])([1-9][0-9])$", message="Must be formatted MM/YY")
    var ccExpiration: String? = null

    @Digits(integer = 3, fraction = 0, message = "Invalid CVV")
    var ccCVV: String? = null

    /** Tracks the identifiers of the tacos added to this order. */
    @ManyToMany(targetEntity = Taco::class)
    val tacos: MutableList<Taco> = mutableListOf()

    @Suppress("unused")
    @PrePersist
    fun placedAt() {
        placedAt = Date()
    }
}

interface OrderRepository : CrudRepository<Order, Long?>

//@Repository
//class JdbcOrderRepository(@Autowired jdbc: JdbcTemplate) : OrderRepository {
//    private val orderInserter = SimpleJdbcInsert(jdbc).withTableName("Taco_Order").usingGeneratedKeyColumns("id")
//    private val orderTacoInserter = SimpleJdbcInsert(jdbc).withTableName("Taco_Order_Tacos")
//    private val objectMapper = ObjectMapper()
//
//    override fun save(order: Order): Order {
//        order.placedAt = Date()
//        val orderId = saveOrderDetails(order)
//        order.id = orderId
//        for (taco in order.tacos) {
//            orderTacoInserter.execute(mapOf("tacoOrder" to orderId, "taco" to taco))
//        }
//        return order
//    }
//
//    private fun saveOrderDetails(order: Order): Long {
//        val values = objectMapper.convertValue(order, MutableMap::class.java) as MutableMap<String, Any?>
//        values["placedAt"] = order.placedAt
//        return orderInserter.executeAndReturnKey(values).toLong()
//    }
//}