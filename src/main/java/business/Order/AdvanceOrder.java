package business.Order;

import java.util.Random;

import model.Country;
import model.Player;

/**
 * Class describes the functionalities of displacing armies.
 * 
 * @author Kevin
 * @author ishaanbajaj
 * @version build 2
 */
public class AdvanceOrder implements Order{

	/**
	 * Object of class Country - the country from where armies are being attacked
	 */
	private Country fromCountry;
	/**
	 * Object of class Country - the country to where armies are being attacked
	 */
	private Country targetCountry;
	/**
	 * number of armies to attack
	 */
	private int to_deploy_armies;
	/**
	 * Object of class Player to get the current player to advance order
	 */
	private Player player;
	/**
	 * Random
	 */
	Random random = new Random();
	/**
	 * attack, defense integer values to get percentage of winning/losing attack.
	 */
	int attack, defense;
	
	/**
	 * Parameterized constructor to build an advance order
	 * @param fromCountry - Source country
	 * @param targetCountry - Destination country
	 * @param to_deploy_armies - Number of armies to advance
	 * @param player - The player that is advance the armies from their country to another country
	 */
	public AdvanceOrder(Country fromCountry, Country targetCountry, int to_deploy_armies, Player player) {
		super();
		this.fromCountry = fromCountry;
		this.targetCountry = targetCountry;
		this.to_deploy_armies = to_deploy_armies;
		this.player = player;
	}
	
	/**
	 * Execute advance order
	 * 1. Check if the source country belongs to the player, if so just advance the armies units
	 * 2. Check if player has a peace treaty with the target country's owner
	 * 3. If none of the above, then attack and defense occurs.  
	 * 
	 * Attack: 60% chance of defeating defending army unit
	 * Defense: 70% chance of defeating attacking army unit
	 * Once attack and defense phase are complete, if units from attacking army is greater than 0 and defending
	 * army units is 0, then the player captures the target country and the remaining attacking army moves to the newly
	 * captured country
	 */
	@Override
	public void execute() {
		
		if(player.getCountriesHold().contains(targetCountry)) {
			targetCountry.armiesDeploy(to_deploy_armies);
			fromCountry.armiesRemove(to_deploy_armies);
		}
		else if(player.getPeaceWith() == targetCountry.getCountryOwner()) {
			System.out.println("You have a peace treaty with: " 
								+ targetCountry.getCountryOwner().getPlayerName() 
								+ " - attack is cancelled on county: "
								+ targetCountry.getCountryId());
		}
		else {
			int totalAttackingArmy = to_deploy_armies;
			int totalDefendingArmy = targetCountry.getArmies();
			
			for(int i = 0; i < totalAttackingArmy; ++i) {
				attack = random.nextInt(10) + 1;
				if(targetCountry.getArmies() == 0) {
					break;
				}
				else if(attack <= 6) {
					targetCountry.armyUnitDefeat();
				}
			}
			
			for(int i = 0; i < totalDefendingArmy; ++i) {
				defense = random.nextInt(10) + 1;
				if(totalAttackingArmy == 0) {
					break;
				}
				else if(defense <= 7) {
					--totalAttackingArmy;
					fromCountry.armyUnitDefeat();
				}
			}
			
			if(totalAttackingArmy > 0 && targetCountry.getArmies() == 0) {
				System.out.println(player.getPlayerName() + " HAS CAPTURED: " + targetCountry.getCountryId());
				fromCountry.armiesRemove(totalAttackingArmy);
				targetCountry.armiesDeploy(totalAttackingArmy);
				
				targetCountry.getCountryOwner().removeCountry(targetCountry);
				
				player.addCountry(targetCountry);
			}
		}
		
	}
	
	
	/**
	 * Check if source country belongs to the player, if not then null
	 * Check if target country is part of the map, if not then null
	 * Check if the number of armies being deployed is valid, cannot deploy more than you have
	 */
	@Override
	public boolean valid() {
		
		if(fromCountry == null || targetCountry == null) {
			return false;
		}else if(to_deploy_armies < fromCountry.getArmies() && to_deploy_armies >= 0) {
			return true;
		}

		return false;
	}

	/**
	 * Print execution of advance order
	 */
	@Override
	public void printOrder() {
		// TODO Auto-generated method stub
		System.out.println("Advance Order " + to_deploy_armies + " armies from " + fromCountry.getCountryId() + " to " + targetCountry.getCountryId());
		System.out.println("***************************************");
	}

}
