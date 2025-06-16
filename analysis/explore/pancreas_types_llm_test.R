panc_chat <- chat_anthropic(
  model = 'claude-3-7-sonnet-20250219',
  api_key = Sys.getenv("ANTHROPIC_API_KEY")
)

panc_chat$chat(
  "Is Pancreatic Adenocarcinoma a type of pancreatic ductal adenocarinoma?"
)

panc_chat$chat(
  "Is Adenosquamous Carcinoma of the Pancreas a type of pancreatic ductal adenocarinoma?"
)

panc_chat$chat(
  "Is acinar cell carinoma a type of pancreatic ductal adenocarinoma?"
)

panc_chat$chat(
  "Is undifferentiated carinoma of the pancreas a type of pancreatic ductal adenocarinoma?"
)
