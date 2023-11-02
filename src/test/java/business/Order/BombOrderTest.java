package business.Order;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import model.Continent;
import model.Country;
import model.MapModel;
import model.Player;

class BombOrderTest {
	
	private BombOrder bomb_order;
	
	private Player P1, P2;
	
	private MapModel d_MM = new MapModel();
	
	private Continent d_America;
	
	private Country d_Canada, d_USA, d_Mexico;
	
	@BeforeEach
	void setUp() {
		
		d_America = new Continent("North America");
		
		d_Canada = new Country("Canada", d_America);
		d_USA = new Country("USA", d_America);
		d_Mexico = new Country("Mexico", d_America);
		
		P1 = new Player("Kevin");
		P2 = new Player("Rohit");
		
		d_MM.addContinent(d_America);
		
		d_MM.addContinentCountries(d_America, d_Canada);
		d_MM.addContinentCountries(d_America, d_USA);
		d_MM.addContinentCountries(d_America, d_Mexico);
		
		d_MM.addBorders(d_Canada, d_USA);
		
		d_MM.addBorders(d_USA, d_Canada);
		d_MM.addBorders(d_USA, d_Mexico);
		
		d_MM.addBorders(d_Mexico, d_USA);
		
		P1.addCountryHold(d_Canada);
		P2.addCountryHold(d_USA);
		
		P1.getCountry(d_Canada).setArmy(5);
		P2.getCountry(d_USA).setArmy(10);
		
		bomb_order = new BombOrder(P1, d_USA);
	}

	@Test
	void testValid() {
		
		assertTrue(bomb_order.valid());
		
	}
	
	@Test
	void testExecute() {
		
		bomb_order.execute();
		
		assertEquals(5, d_USA.getArmies());
		
	}

}
