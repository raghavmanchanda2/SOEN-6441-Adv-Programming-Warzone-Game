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
import model.ResponseWrapper;

class CheaterStrategyTest {

    private CheaterStrategy d_CheaterStrategy;
    private Player d_Player;
    private MapModel d_MapModel;
    private Continent d_America;
    private Country d_Canada, d_USA, d_Mexico;

    @BeforeEach
    void setUp() {
        d_Player = new Player("Kevin");
        d_MapModel = new MapModel();
        d_CheaterStrategy = new CheaterStrategy(d_Player, d_MapModel, null, null);
        d_America = new Continent("North America");
        d_Canada = new Country("Canada", d_America);
        d_USA = new Country("USA", d_America);
        d_Mexico = new Country("Mexico", d_America);

    }

    @Test
    void testGetStrategyName() {
        assertEquals("CHEATER", d_CheaterStrategy.getStrategyName());
    }
}
