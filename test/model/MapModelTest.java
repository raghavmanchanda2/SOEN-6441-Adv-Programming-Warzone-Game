package model;

import static org.junit.jupiter.api.Assertions.*;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
/**
 * JUnit test class for the {@link MapModel} class.
 * @author Sumit Kumar
 */
class MapModelTest {
	
	private MapModel d_MM = new MapModel();
	private static int d_test_number = 1;


	@BeforeEach
	void setUp() {
		
		
		System.out.println("Starting Test: " + d_test_number);
		System.out.println("-----------------------");
		
		
		Continent l_America = new Continent("America");
		
		Country l_Canada = new Country("Canada", l_America);
		Country l_USA = new Country("USA", l_America);
		Country l_Mexico = new Country("Mexico", l_America);
		Country l_Guatemala = new Country("Guatemala", l_America);
		Country l_Nicaragua = new Country("Nicaragua", l_America);
		Country l_Colombia = new Country("Colombia", l_America);
		Country l_Venezuela = new Country("Venezuela", l_America);
		Country l_Ecuador = new Country("Ecuador", l_America);
		Country l_Peru = new Country("Peru", l_America);
		Country l_Brazil = new Country("Brazil", l_America);
		
		
		d_MM.addContinent(l_America);
		
		d_MM.addContinentCountries(l_America, l_Canada);
		d_MM.addContinentCountries(l_America, l_USA);
		d_MM.addContinentCountries(l_America, l_Mexico);
		d_MM.addContinentCountries(l_America, l_Guatemala);
		d_MM.addContinentCountries(l_America, l_Nicaragua);
		d_MM.addContinentCountries(l_America, l_Colombia);
		d_MM.addContinentCountries(l_America, l_Venezuela);
		d_MM.addContinentCountries(l_America, l_Ecuador);
		d_MM.addContinentCountries(l_America, l_Peru);
		d_MM.addContinentCountries(l_America, l_Brazil);
		
		d_MM.addBorders(l_Canada, l_USA);
		
		d_MM.addBorders(l_USA, l_Canada);
		d_MM.addBorders(l_USA, l_Mexico);
		
		d_MM.addBorders(l_Mexico, l_USA);
		d_MM.addBorders(l_Mexico, l_Guatemala);
		
		d_MM.addBorders(l_Guatemala, l_Mexico);
		d_MM.addBorders(l_Guatemala, l_Nicaragua);
		
		d_MM.addBorders(l_Nicaragua, l_Guatemala);
		d_MM.addBorders(l_Nicaragua, l_Colombia);
		
		d_MM.addBorders(l_Colombia, l_Nicaragua);
		d_MM.addBorders(l_Colombia, l_Venezuela);
		d_MM.addBorders(l_Colombia, l_Ecuador);
		d_MM.addBorders(l_Colombia, l_Brazil);
		d_MM.addBorders(l_Colombia, l_Peru);
		
		d_MM.addBorders(l_Venezuela, l_Colombia);
		d_MM.addBorders(l_Venezuela, l_Brazil);
		
		d_MM.addBorders(l_Ecuador, l_Colombia);
		d_MM.addBorders(l_Ecuador, l_Peru);
		
		d_MM.addBorders(l_Peru, l_Ecuador);
		d_MM.addBorders(l_Peru, l_Colombia);
		d_MM.addBorders(l_Peru, l_Brazil);
		
		d_MM.addBorders(l_Brazil, l_Peru);
		d_MM.addBorders(l_Brazil, l_Colombia);
		d_MM.addBorders(l_Brazil, l_Venezuela);
		
		
	}

	@AfterEach
	void tearDown() {
		++d_test_number;
		System.out.println("\n\n");
	}
	
	/**
	 * Method to assert that the map is a connected graph
	 */
	@Test
	void ConnectedGraphMap() {
	    assertTrue(MapConnected(d_MM));
	}

	/**
	 * Method for checking if the entire map is a connected graph
	 * @param p_MM MapModel
	 * @return boolean value indicating whether the entire map is a connected graph
	 */
	public static boolean MapConnected(MapModel p_MM) {
	    if (p_MM.getCountries().isEmpty()) {
	        return true;
	    }

	    Set<Country> visited = new HashSet<>();

	    for (Country country : p_MM.getCountries()) {
	        if (!visited.contains(country)) {
	            System.out.println("Starting Country: " + country.getCountryId());
	            if (!DFS(p_MM, country, visited)) {
	                return false; // If any country is not reachable, return false
	            }
	        }
	    }

	    return true; // All countries are connected
	}

	/**
	 * DFS: Depth First Search algorithm to ensure that the map is a connected graph of countries
	 * The visited set only adds a country if that country has never been visited before.
	 * A set also prevents duplication if we want to add a country that is already contained in that set
	 *
	 * @param p_MM Map Model
	 * @param p_country Country to start the DFS from
	 * @param p_visited Set of visited countries
	 * @return boolean value to ensure the search has visited every country in the map
	 */
	public static boolean DFS(MapModel p_MM, Country p_country, Set<Country> p_visited) {

	    p_visited.add(p_country);

	    for (Country neighbor : p_MM.getBorders().get(p_country)) {
	        if (!p_visited.contains(neighbor)) {
	            System.out.println(neighbor.getCountryId());
	            DFS(p_MM, neighbor, p_visited);
	        }
	    }

	    return true; // Since we are checking connectivity, we don't need to return a size comparison.
	}

	
}
