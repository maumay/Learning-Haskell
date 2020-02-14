package com.github.maumay.tacocloud

import org.springframework.data.repository.CrudRepository
import javax.persistence.Entity
import javax.persistence.EnumType
import javax.persistence.Enumerated
import javax.persistence.Id

enum class IngredientType { WRAP, PROTEIN, VEGGIES, CHEESE, SAUCE }

@Entity
data class Ingredient(@Id val id: String?, val name: String?, @Enumerated(EnumType.STRING) val type: IngredientType?) {
    @Suppress("unused")
    private constructor() : this(null, null, null)
}

interface IngredientRepository : CrudRepository<Ingredient, String?>


//@Repository
//class JdbcIngredientRepository(@Autowired private val jdbc: JdbcTemplate) : IngredientRepository {
//    override fun findOne(id: String): Ingredient? {
//        return jdbc.queryForObject("select id, name, type from Ingredient where id=$id", this::mapRowToIngredient)
//    }
//
//    override fun save(ingredient: Ingredient): Ingredient {
//        jdbc.update("insert into Ingredient (id, name, type) values (${ingredient.id}, ${ingredient.name}, ${ingredient.type})")
//        return ingredient
//    }
//
//    override fun findAll(): Iterable<Ingredient> {
//        return jdbc.query("select id, name, type from Ingredient", this::mapRowToIngredient)
//    }
//
//    private fun mapRowToIngredient(rs: ResultSet, index: Int): Ingredient {
//        return Ingredient(rs.getString("id"), rs.getString("name"), IngredientType.valueOf(rs.getString("type")))
//    }
//}