package com.github.maumay.tacocloud

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.jdbc.core.JdbcTemplate
import org.springframework.jdbc.core.PreparedStatementCreatorFactory
import org.springframework.jdbc.support.GeneratedKeyHolder
import org.springframework.stereotype.Repository
import java.sql.Types
import java.util.*
import javax.validation.constraints.NotNull
import javax.validation.constraints.Size

class Taco {
    var id: Long? = null
    var createdAt: Date? = null

    @NotNull
    @Size(min=5, message="Name must be at least 5 characters long.")
    var name: String? = null

    @Size(min=1, message="Must be at least one ingredient.")
    var ingredients: List<String> = listOf()

    override fun toString(): String {
        return "Taco[name=$name, ingredients=$ingredients]"
    }
}

interface TacoRepository {
    fun save(design: Taco): Taco
}

@Repository
class JdbcTacoRepository(@Autowired val jdbc: JdbcTemplate) : TacoRepository{
    override fun save(design: Taco): Taco {
        val id = saveTacoInfo(design)
        design.id = id
        for (ingredient in design.ingredients) {
            jdbc.update("insert into Taco_Ingredients (taco, ingredient) values ($id, $ingredient")
        }
        return design
    }

    private fun saveTacoInfo(design: Taco): Long {
        design.createdAt = Date()
        val pscf = PreparedStatementCreatorFactory("insert into Taco (name, createdAt) values (${Types.VARCHAR}, ${Types.TIMESTAMP})")
        val keyHolder = GeneratedKeyHolder()
        jdbc.update(pscf.newPreparedStatementCreator(listOf(design.name, design.createdAt)), keyHolder)
        return keyHolder.key!!.toLong()
    }
}