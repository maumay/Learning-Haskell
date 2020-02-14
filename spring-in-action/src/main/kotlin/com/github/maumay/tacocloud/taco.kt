package com.github.maumay.tacocloud

import org.springframework.data.repository.CrudRepository
import java.util.*
import javax.persistence.*
import javax.validation.constraints.NotNull
import javax.validation.constraints.Size

@Entity
class Taco {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    var id: Long? = null
    var createdAt: Date? = null

    @NotNull
    @Size(min=5, message="Name must be at least 5 characters long.")
    var name: String? = null

    @ManyToMany(targetEntity = Ingredient::class)
    @Size(min=1, message="Must be at least one ingredient.")
    var ingredients: List<Ingredient> = listOf()

    @Suppress("unused")
    @PrePersist
    fun createdAt() {
        createdAt = Date()
    }

    override fun toString(): String {
        return "Taco[name=$name, ingredients=$ingredients]"
    }
}

interface TacoRepository : CrudRepository<Taco, Long?>

//@Repository
//class JdbcTacoRepository(@Autowired val jdbc: JdbcTemplate) : TacoRepository{
//    override fun save(design: Taco): Taco {
//        val id = saveTacoInfo(design)
//        design.id = id
//        for (ingredient in design.ingredients) {
//            jdbc.update("insert into Taco_Ingredients (taco, ingredient) values (?, ?)", id, ingredient)
//        }
//        return design
//    }
//
//    private fun saveTacoInfo(design: Taco): Long {
//        design.createdAt = Date()
//        val pscf = PreparedStatementCreatorFactory(
//                "insert into Taco (name, createdAt) values (?, ?)", Types.VARCHAR, Types.TIMESTAMP)
//        pscf.setReturnGeneratedKeys(true)
//        val keyHolder = GeneratedKeyHolder()
//        jdbc.update(pscf.newPreparedStatementCreator(listOf(design.name, Timestamp(design.createdAt!!.time))), keyHolder)
//        return keyHolder.key!!.toLong()
//    }
//}