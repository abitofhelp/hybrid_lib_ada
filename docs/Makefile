# Makefile for hybrid_lib_docs
# Diagram generation using local PlantUML jar

PLANTUML_JAR := tools/puml/plantuml.jar
JAVA := java

# Colors for output
CYAN := \033[36m
GREEN := \033[32m
YELLOW := \033[33m
NC := \033[0m

.PHONY: help diagrams clean

help: ## Show this help
	@echo "Available targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

diagrams: ## Generate SVG diagrams from PlantUML sources
	@echo "$(CYAN)Generating SVG diagrams from PlantUML...$(NC)"
	@if [ ! -f "$(PLANTUML_JAR)" ]; then \
		echo "Error: $(PLANTUML_JAR) not found"; \
		exit 1; \
	fi
	@find diagrams -name "*.puml" | while read f; do \
		echo "  Processing $$f..."; \
		$(JAVA) -jar $(PLANTUML_JAR) -tsvg "$$f"; \
	done
	@echo "$(GREEN)Diagrams generated$(NC)"

clean: ## Remove generated SVG files
	@echo "$(YELLOW)Removing generated SVG files...$(NC)"
	@find diagrams -name "*.svg" -delete
	@echo "$(GREEN)Clean complete$(NC)"

.DEFAULT_GOAL := help
