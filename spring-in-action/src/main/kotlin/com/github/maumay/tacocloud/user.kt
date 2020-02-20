package com.github.maumay.tacocloud

import org.springframework.beans.factory.annotation.Autowired
import org.springframework.data.repository.CrudRepository
import org.springframework.security.core.GrantedAuthority
import org.springframework.security.core.authority.SimpleGrantedAuthority
import org.springframework.security.core.userdetails.UserDetails
import org.springframework.security.core.userdetails.UserDetailsService
import org.springframework.security.core.userdetails.UsernameNotFoundException
import org.springframework.security.crypto.password.PasswordEncoder
import org.springframework.stereotype.Service
import java.util.*
import javax.persistence.Entity
import javax.persistence.GeneratedValue
import javax.persistence.GenerationType
import javax.persistence.Id

@Entity
class User(private val username: String?,
           private val password: String?,
           val fullname: String?,
           val street: String?,
           val city: String?,
           val state: String?,
           val zip: String?,
           val phoneNumber: String?) : UserDetails {

    @Suppress("unused")
    private constructor() : this(null, null, null, null, null, null, null, null)

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    var id: Long? = null

    override fun getAuthorities(): MutableCollection<out GrantedAuthority> {
        return mutableListOf(SimpleGrantedAuthority("ROLE_USER"))
    }

    override fun isEnabled(): Boolean {
        return true
    }

    override fun getUsername(): String {
        return username!!
    }

    override fun isCredentialsNonExpired(): Boolean {
        return true
    }

    override fun getPassword(): String {
        return password!!
    }

    override fun isAccountNonExpired(): Boolean {
        return true
    }

    override fun isAccountNonLocked(): Boolean {
        return true
    }
}

interface UserRepository : CrudRepository<User, Long?> {
    fun findByUsername(username: String?): Optional<User>
}

@Service
class UserDetailsServiceImpl(@Autowired private val userRepository: UserRepository) : UserDetailsService {
    override fun loadUserByUsername(username: String?): UserDetails {
        return userRepository.findByUsername(username).orElseThrow { UsernameNotFoundException("User '$username' not found") }
    }
}

class RegistrationForm {
    var username: String? = null
    var password: String? = null
    var fullname: String? = null
    var street: String? = null
    var city: String? = null
    var state: String? = null
    var zip: String? = null
    var phone: String? = null

    fun toUser(encoder: PasswordEncoder): User {
        return User(username, encoder.encode(password), fullname, street, city, state, zip, phone)
    }
}