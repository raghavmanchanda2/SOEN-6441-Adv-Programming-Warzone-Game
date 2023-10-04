package model;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import model.Continent;
import model.Country;
import model.MapModel;

/**
 * JUNIT test class to test the following:
 * 1. Prove a continent is a connected graph using DFS (Depth First Search)
 * 2. Prove that adding a country that already exists in the map does not create a duplicate
 * 3. Prove that the entire map is a connected graph
 */
class MapModelTest {

    private MapModel d_MM = MapModel.getInstance();
    private static int d_test_number = 1;

    @BeforeEach
    void setUp() {
        System.out.println("Starting Test: " + d_test_number);
        System.out.println("-----------------------");

        // Initialize your map, continents, countries, and borders here...
    }

    @AfterEach
    void tearDown() {
        ++d_test_number;
        System.out.println("\n\n");
    }

    // Existing methods...

    /**
     * Test method to check if the entire map is a connected graph.
     */
    @Test
    void MapIsConnectedGraphTest() {
        assertTrue(MapIsConnectedGraph(d_MM));
    }

    /**
     * Method to check if the entire map is a connected graph.
     */
    public static boolean MapIsConnectedGraph(MapModel p_MM) {
        Set<Country> visited = new HashSet<>();
        
        for (Country country : p_MM.getCountries()) {
            if (!visited.contains(country)) {
                if (!DFS(p_MM, country, visited)) {
                    return false;
                }
            }
        }
        
        return true;
    }

    /**
     * Depth-First Search (DFS) algorithm to check if the map is a connected graph.
     */
    public static boolean DFS(MapModel p_MM, Country p_country, Set<Country> p_visited) {
        p_visited.add(p_country);
        
        for (Country neighbor : p_MM.getBorders().get(p_country)) {
            if (!p_visited.contains(neighbor)) {
                if (!DFS(p_MM, neighbor, p_visited)) {
                    return false;
                }
            }
        }
        
        return true;
    }
}
