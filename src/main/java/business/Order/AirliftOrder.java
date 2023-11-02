package business.Order;

import model.Player;
import model.Country;

/**
 * 
 * Airlift that allows a single transfer of armies from any of your territories.
 * Must wait next turn to attack.
 * Transfer occurs before attack takes place.
 * 
 * @author Kevin
 * @version build 2
 */
public class AirliftOrder implements Order{
	
	private Country originCountry;
	private Country destinationCountry;
	private int armies_to_move;
	private Player player;
	
	public AirliftOrder(Player player, Country countryFrom, Country countryTo, int armies_to_move) {
		this.player = player;
		originCountry = countryFrom;
		destinationCountry = countryTo;
		this.armies_to_move = armies_to_move;
	}

	@Override
	public void execute() {
		int originCountryArmyBefore = originCountry.getArmies();
		int originCountryArmyAfter = originCountryArmyBefore - armies_to_move;
		originCountry.setArmy(originCountryArmyAfter);
		
		int destinationCountryArmyBefore = destinationCountry.getArmies();
		int destinationCountryArmyAfter = destinationCountryArmyBefore + armies_to_move;
		destinationCountry.setArmy(destinationCountryArmyAfter);
	}

	@Override
	public boolean valid() {
		
		if(originCountry == null || destinationCountry == null
		|| !player.getCountriesHold().contains(originCountry) 
		|| !player.getCountriesHold().contains(destinationCountry)
		|| originCountry == destinationCountry) {
			return false;
		}
		else 
		if(originCountry.getArmies() <= armies_to_move && armies_to_move > 0) {
			return true;
		}
		
		return false;
	}

	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		
	}

}