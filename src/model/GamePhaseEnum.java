package model;

import java.util.List;

public enum GamePhaseEnum {
    EditMapPhase{

		@Override
		public List<GamePhaseEnum> allStates() {
			// TODO Auto-generated method stub
			return null;
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
