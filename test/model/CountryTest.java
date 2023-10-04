package model;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
/**
 * JUnit test class for the {@link Country} class.
 * @author Sumit Kumar
 */
class CountryTest {
    private Country d_country;

    @BeforeEach
    void setUp() {
        d_country = new Country("Canada");
    }

    @Test
    void testGetCountryId() {
        assertEquals("Canada", d_country.getCountryId(), "Country ID should match");
    }

    @Test
    void testSetAndGetContinent() {
        Continent d_continent = new Continent("North America");
        d_country.setContinent(d_continent);
        assertEquals(d_continent, d_country.getContinent(), "Continent should be set correctly");
    }

    @Test
    void testGetArmies() {
        assertEquals(0, d_country.getArmies(), "Initially, the number of armies should be 0");
    }

    @Test
    void testArmiesDeploy() {
        d_country.armiesDeploy(5);
        assertEquals(5, d_country.getArmies(), "Deployed armies should be added correctly");
    }

    @Test
    void testGetNeighbors() {
        Country d_neighbor1 = new Country("USA");
        Country d_neighbor2 = new Country("Mexico");

        // Directly add neighbors to the country
        d_country.getNeighbors().add(d_neighbor1);
        d_country.getNeighbors().add(d_neighbor2);

        assertEquals(2, d_country.getNeighbors().size(), "Should return the correct number of neighbors");
        assertTrue(d_country.getNeighbors().contains(d_neighbor1), "Should contain neighbor1");
        assertTrue(d_country.getNeighbors().contains(d_neighbor2), "Should contain neighbor2");
    }

    @Test
    void testUniqueCountryId() {
        d_country.setUniqueCountryId(42);
        assertEquals(42, d_country.getUniqueCountryId(), "Unique Country ID should be set correctly");
    }

    @Test
    void testConnectedCountries() {
        Map<Country, List<Country>> d_connectedCountries = new HashMap<>();
        d_connectedCountries.put(new Country("USA"), new ArrayList<>());
        d_connectedCountries.put(new Country("Mexico"), new ArrayList<>());
        d_country.setConnectedCountries(d_connectedCountries);
        assertEquals(d_connectedCountries, d_country.getConnectedCountries(), "Connected countries should be set correctly");
    }

    @Test
    void testCountryName() {
        d_country.setD_countryName("NewCountryName");
        assertEquals("NewCountryName", d_country.getD_countryName(), "Country name should be set correctly");
    }
}
