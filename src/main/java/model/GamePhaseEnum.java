package model;

import java.util.Collections;
import java.util.List;
import controller.*;

/**
 * Enum to manage all the phases of WarZone
 * Possible States - EditMapPhase, GameLoadPhase, GameExitPhase
 *
 * @author Ishaan Bajaj
 * @version 1.0.1
 */

public enum GamePhaseEnum {
    EditMapPhase{

		@Override
		public List<GamePhaseEnum> allStates() {
			// TODO Auto-generated method stub
			return Collections.singletonList(StartUp);
		}

		@Override
		public WarzoneController getController() {
			// TODO Auto-generated method stub
			return new EditMapPhase();
		}

    },
	GameLoadPhase{

		@Override
		public List<GamePhaseEnum> allStates() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public WarzoneController getController() {
			// TODO Auto-generated method stub
			return null;
		}

	},
	StartUp{
		@Override
		public List<GamePhaseEnum> allStates() {
			return null;
		}

		/**
		 * for game play phase
		 * @return CurrentGamePlay object
		 */
		@Override
		public WarzoneController getController() {
			return null;
		}
	},
	GameExitPhase{

		@Override
		public List<GamePhaseEnum> allStates() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public WarzoneController getController() {
			// TODO Auto-generated method stub
			return null;
		}
		
	};
	
	
	public abstract List<GamePhaseEnum> allStates();
	
	public abstract WarzoneController getController();
}
