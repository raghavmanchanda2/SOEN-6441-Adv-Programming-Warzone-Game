package Strategy;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import model.Player;
import model.MapModel;
import model.Country;
import model.Continent;
import controller.MainPlayPhaseController;
import business.MainPlayPhaseBusinessCommands;
import logger.GeneralException;
import model.ResponseWrapper;

class CheaterStrategyTest {

    private CheaterStrategy d_CheaterStrategy;
    private Player d_Player;
    private MapModel d_MapModel;
    private Continent d_Continent;
    private Country d_Country1, d_Country2;

    @BeforeEach
    void setUp() {
        d_Player = new Player("PlayerName");
        d_MapModel = new MapModel();
        d_CheaterStrategy = new CheaterStrategy(d_Player, d_MapModel, null, null);
        d_Continent = new Continent("Continent");
        d_Country1 = new Country("Country1", d_Continent);
        d_Country2 = new Country("Country2", d_Continent);
        // Further setup for countries and continents
    }

    @Test
    void testGetStrategyName() {
        assertEquals("CHEATER", d_CheaterStrategy.getStrategyName(), "Strategy name should match 'CHEATER'.");
    }

}
