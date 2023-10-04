package model;
import static org.junit.jupiter.api.Assertions.*;

import model.Country;
import model.MapModel;
import model.Continent; // Make sure to import Continent
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

public class PlayerTest {

    private Player player;
    private MapModel mapModel;

    @BeforeEach
    public void setUp() {
        player = new Player(null); // PlayerStrategy can be mocked for testing purposes
        player.setReinforcementArmies(0); // Reset reinforcement armies before each test
    }

    @Test
    public void testCalculateReinforcementArmies() {
        List<Country> capturedCountries = createCapturedCountries();
        player.setCapturedCountries(capturedCountries);

        // Create a sample continent for testing
        Continent testContinent = new Continent("TestContinent");

        // Create a sample MapModel and add the test continent to it
        mapModel = new MapModel();
        mapModel.addContinent(testContinent);

        player.calculateReinforcementArmies(mapModel);

        // Calculate the expected reinforcement armies based on the test data
        int expectedReinforcementArmies = (int) Math.floor(capturedCountries.size() / 3f);

        assertEquals(expectedReinforcementArmies, player.getReinforcementArmies());
    }

    // Helper method to create a list of captured countries for testing
    private List<Country> createCapturedCountries() {
        List<Country> capturedCountries = new ArrayList<>();
        capturedCountries.add(new Country("Country1"));
        capturedCountries.add(new Country("Country2"));
        capturedCountries.add(new Country("Country3"));
        // Add more countries as needed
        return capturedCountries;
    }
}
