module ParseJSONSpec (spec) where


import Test.Hspec

spec :: Spec
spec = do
  it "parses simpleModel.slx" $ do
    pendingWith "need to figure out how to compile the model as a precondition"
