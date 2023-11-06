package business.Order;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import model.Card;
import model.Continent;
import model.Country;
import model.GameModel;
import model.MapModel;
import model.Player;
import model.Card.CardType;

class DiplomacyOrderTest {

	private DiplomacyOrder diplomacy_order;
	
	private Player P1, P2;
	
	private MapModel d_MM = MapModel.getInstance();
	
	private Continent d_America;
	
	private Country d_Canada, d_USA, d_Mexico;
	
	@BeforeEach
	void setUp() throws Exception {
		
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
		
		Card c = new Card(CardType.DIPLOMACY);
		
		P1.addSpecificCard(c);
		
		diplomacy_order = new DiplomacyOrder(P1, P2);
		
	}

	@Test
	void testValid() {
		System.out.println("Before Validation Stage");
		P1.printCardList();
		
		assertTrue(diplomacy_order.valid());
		
		System.out.println("After Validation Stage");
		P1.printCardList();
	}
	
	@Test
	void testExecute() {
		
		diplomacy_order.execute();
		
		assertEquals(P1.getPeaceWith(), P2);
		
	}

}
