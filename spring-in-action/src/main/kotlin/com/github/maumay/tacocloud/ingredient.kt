package com.github.maumay.tacocloud

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.stereotype.Repository
import java.sql.ResultSet
import javax.persistence.Entity
import javax.persistence.Id

enum class IngredientType { WRAP, PROTEIN, VEGGIES, CHEESE, SAUCE }

@Entity
data class Ingredient(@Id val id: String, val name: String, val type: IngredientType) {
    @Suppress("unused")
    private constructor() : this("", "", IngredientType.WRAP)
}

interface IngredientRepository {
    fun findAll(): Iterable<Ingredient>
    fun findOne(id: String): Ingredient?
    fun save(ingredient: Ingredient): Ingredient
}
@Repository
class JdbcIngredientRepository(@Autowired private val jdbc: JdbcTemplate) : IngredientRepository {
    override fun findOne(id: String): Ingredient? {
        return jdbc.queryForObject("select id, name, type from Ingredient where id=$id", this::mapRowToIngredient)
    }

    override fun save(ingredient: Ingredient): Ingredient {
        jdbc.update("insert into Ingredient (id, name, type) values (${ingredient.id}, ${ingredient.name}, ${ingredient.type})")
        return ingredient
    }

    override fun findAll(): Iterable<Ingredient> {
        return jdbc.query("select id, name, type from Ingredient", this::mapRowToIngredient)
    }

    private fun mapRowToIngredient(rs: ResultSet, index: Int): Ingredient {
        return Ingredient(rs.getString("id"), rs.getString("name"), IngredientType.valueOf(rs.getString("type")))
    }
}