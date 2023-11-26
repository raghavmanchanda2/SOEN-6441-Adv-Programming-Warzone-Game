package Strategy;

import business.MainPlayPhaseBusinessCommands;
import business.Order.AdvanceOrder;
import business.Order.DeployOrder;
import business.Order.Order;
import controller.MainPlayPhaseController;
import logger.GeneralException;
import model.Country;
import model.MapModel;
import model.Player;
import model.ResponseWrapper;

public class BenevolentStrategy extends PlayerStrategy{

	private final String strategyName = "HUMAN";
	static int phase = 1;

	private Country strongest;
	private Country weakest;

	@Override
	public String getStrategyName() {

		return strategyName;
	}

	public BenevolentStrategy(Player p_player, MapModel p_mapModel, MainPlayPhaseController p_mainPlayPhaseController, MainPlayPhaseBusinessCommands p_mainPlayPhaseBusinessCommands) {
		super(p_player, p_mapModel, p_mainPlayPhaseController, p_mainPlayPhaseBusinessCommands);
	}

	@Override
	public ResponseWrapper createOrder() throws GeneralException{

		ResponseWrapper response;
		
		if(phase == 1) {
			
			Country toDeploy = toDefend();
			
			response = d_mainPlayPhaseBusinessCommands.deploy(d_player, toDeploy.getCountryId(), d_player.getArmiesToIssue());
		
				++phase;
				return response;
			
		}
		else if(phase == 2) {

			strongest = getStrongest();
			weakest = getWeakest();

			response = d_mainPlayPhaseBusinessCommands.advance(d_player, strongest.getCountryId(), weakest.getCountryId(), strongest.getArmies()/2);

			
				phase = 1;
				d_player.performCommit();
				return response;
			
		}



		return null;
	}

	@Override
	protected Country toAttack() {
		return null;
	}

	@Override
	protected Country toAttackFrom() {
		return null;
	}

	@Override
	protected Country toMoveFrom() {

		strongest = getStrongest();
		weakest = strongest;

		return strongest;
	}

	@Override
	protected Country toMoveTo() {

		for(Country country : strongest.getNeighbors()) {
			if(country.getArmies() < weakest.getArmies()) {
				weakest = country;
			}
		}

		if(weakest == strongest) {
			return null;
		}
		else return weakest;
	}

	@Override
	protected Country toDefend() {

		Country weakest = getWeakest();

		return weakest;
	}

	private Country getWeakest() {

		Country weakest = d_player.getCountriesHold().get(0);

		for(Country country : d_player.getCountriesHold()) {
			if(weakest.getArmies() > country.getArmies()) {
				weakest = country;
			}
		}

		return weakest;

	}

	private Country getStrongest() {

		Country strongest = d_player.getCountriesHold().get(0);

		for(Country country : d_player.getCountriesHold()) {
			if(strongest.getArmies() < country.getArmies()) {
				strongest = country;
			}
		}

		return strongest;

	}

}

























