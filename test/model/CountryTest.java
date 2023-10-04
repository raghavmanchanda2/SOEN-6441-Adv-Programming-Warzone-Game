package model;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * JUnit test class for the {@link Country} class.
 */
public class CountryTest {
    private Country country;
    private Continent continent;
    private Map<Country, List<Country>> connectedCountries;

    @Before
    public void setUp() {
        country = new Country(1, "TestCountry");
        continent = new Continent("TestContinent");
        connectedCountries = new HashMap<>();
    }

    /**
     * Test the {@link Country#getUniqueCountryId()} method.
     */
    @Test
    public void testGetUniqueCountryId() {
        assertEquals(1, country.getUniqueCountryId());
    }

    /**
     * Test the {@link Country#setUniqueCountryId(int)} method.
     */
    @Test
    public void testSetUniqueCountryId() {
        country.setUniqueCountryId(2);
        assertEquals(2, country.getUniqueCountryId());
    }

    /**
     * Test the {@link Country#getCountryId()} method.
     */
    @Test
    public void testGetCountryId() {
        assertEquals("TestCountry", country.getCountryId());
    }

    /**
     * Test the {@link Country#setCountryId(String)} method.
     */
    @Test
    public void testSetCountryId() {
        country.setCountryId("UpdatedCountry");
        assertEquals("UpdatedCountry", country.getCountryId());
    }

    /**
     * Test the {@link Country#getContinent()} method.
     */
    @Test
    public void testGetContinent() {
        assertNull(country.getContinent());
    }

    /**
     * Test the {@link Country#setContinent(Continent)} method.
     */
    @Test
    public void testSetContinent() {
        country.setContinent(continent);
        assertEquals(continent, country.getContinent());
    }

    /**
     * Test the {@link Country#getConnectedCountries()} method.
     */
    @Test
    public void testGetConnectedCountries() {
        assertNull(country.getConnectedCountries());
    }

    /**
     * Test the {@link Country#setConnectedCountries(Map)} method.
     */
    @Test
    public void testSetConnectedCountries() {
        List<Country> neighbors = new ArrayList<>();
        neighbors.add(new Country("Neighbor1"));
        neighbors.add(new Country("Neighbor2"));
        connectedCountries.put(country, neighbors);

        country.setConnectedCountries(connectedCountries);

        assertNotNull(country.getConnectedCountries());
        assertEquals(neighbors, country.getConnectedCountries().get(country));
    }

    /**
     * Test setting connected countries to null using {@link Country#setConnectedCountries(Map)}.
     */
    @Test
    public void testSetConnectedCountriesWithNull() {
        country.setConnectedCountries(null);
        assertNull(country.getConnectedCountries());
    }

    /**
     * Test setting connected countries to an empty map using {@link Country#setConnectedCountries(Map)}.
     */
    @Test
    public void testSetConnectedCountriesWithEmptyMap() {
        country.setConnectedCountries(new HashMap<>());
        assertTrue(country.getConnectedCountries().isEmpty());
    }

    /**
     * Test setting connected countries with a circular reference using {@link Country#setConnectedCountries(Map)}.
     */
    @Test
    public void testSetConnectedCountriesWithCircularReference() {
        List<Country> neighbors = new ArrayList<>();
        neighbors.add(country); // Adding the country itself as a neighbor
        connectedCountries.put(country, neighbors);

        // This should not cause a stack overflow or infinite loop
        country.setConnectedCountries(connectedCountries);

        assertNotNull(country.getConnectedCountries());
        assertEquals(neighbors, country.getConnectedCountries().get(country));
    }
}
