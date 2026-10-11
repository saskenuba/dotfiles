#!/usr/bin/env zsh

# My Custom Functions Plugin for Oh My Zsh
# Author: Martin
# Version: 0.0.1

# NPM nuclear reset function
npm_nuclear_reset() {
    # Check if we're in a node project
    if [[ ! -f "package.json" ]]; then
        echo "❌ No package.json found. Are you in a Node.js project directory?"
        return 1
    fi
    
    echo "🚨 This will delete node_modules, package-lock.json, and npm cache"
    echo "📁 Current directory: $(pwd)"
    read "?Continue? (y/N): " confirm
    
    if [[ $confirm =~ ^[Yy]$ ]]; then
        echo "# Remove everything npm-related"
        rm -rf node_modules
        rm -rf package-lock.json  
        rm -rf ~/.npm
        npm cache clean --force
        echo "✅ NPM nuclear reset complete!"
        echo "💡 Run 'npm install' to reinstall dependencies"
    else
        echo "❌ Cancelled"
    fi
}

# AWS Profile switching and credential export functions

# Function to switch to production profile and export credentials
dataico-prod() {
    echo "🔄 Switching to production AWS profile..."
    export AWS_PROFILE=prod-agent
    export AWS_REGION=${AWS_REGION:-us-east-1}  # Default region if not set
    echo "✅ AWS_PROFILE set to: $AWS_PROFILE"

    # Export credentials using your existing script logic
    _export_aws_credentials
}

# Function to switch to staging profile and export credentials
dataico-staging() {
    echo "🔄 Switching to staging AWS profile..."
    export AWS_PROFILE=staging-agent
    export AWS_REGION=${AWS_REGION:-us-east-1}  # Default region if not set
    echo "✅ AWS_PROFILE set to: $AWS_PROFILE"

    # Export credentials using your existing script logic
    _export_aws_credentials
}

# Helper function that contains your credential export logic
_export_aws_credentials() {
    # Check if AWS CLI is available
    if ! command -v aws &> /dev/null; then
        echo "❌ AWS CLI not found. Please install it first."
        return 1
    fi

    # Check if jq is available
    if ! command -v jq &> /dev/null; then
        echo "❌ jq not found. Please install it first (brew install jq)."
        return 1
    fi

    echo "🔑 Checking AWS SSO login status..."

    # Check if already logged in by trying to get caller identity
    aws sts get-caller-identity --profile $AWS_PROFILE >/dev/null 2>&1
    login_check="$?"

    if [ $login_check -ne 0 ]; then
        echo "🔐 Not logged in. Running AWS SSO login..."
        aws sso login --profile $AWS_PROFILE
        if [ $? -ne 0 ]; then
            echo "❌ SSO login failed"
            return 1
        fi
    else
        echo "✅ Already logged in to AWS SSO"
    fi

    echo "🔑 Exporting AWS credentials for profile: $AWS_PROFILE"

    # Try to export credentials
    cred=$(aws configure export-credentials 2>/dev/null)
    cred_success="$?"
    success="0"

    echo "📤 Output code for exporting credentials: ${cred_success}"

    if [ $cred_success != $success ]; then
        echo "❌ Failed to export credentials after login"
        return 1
    fi

    if [ $cred_success -eq $success ]; then
        echo ""
        echo "==="
        echo ""
        echo "🎯 The credentials for ${AWS_PROFILE} on ${AWS_REGION} are:"
        echo ""

        key_id=$(echo $cred | jq -r '.AccessKeyId')
        secret_key=$(echo $cred | jq -r '.SecretAccessKey')
        session_token=$(echo $cred | jq -r '.SessionToken')

        intellij_env_vars="AWS_ACCESS_KEY_ID=${key_id}\nAWS_SECRET_ACCESS_KEY=${secret_key}\nAWS_SESSION_TOKEN=${session_token}\nAWS_REGION=us-east-1\n"

        printf "$intellij_env_vars"

        # Copy to clipboard (works on macOS)
        if command -v pbcopy &> /dev/null; then
            printf "$intellij_env_vars" | pbcopy
            echo ""
            echo "✅ Credentials successfully copied to your clipboard!"
        else
            echo ""
            echo "ℹ️  Clipboard copy not available (pbcopy not found)"
        fi
    else
        echo "❌ Failed to export credentials"
        return 1
    fi
}

# Switch AWS profile/region and log in via SSO (no credential export)
aws-prod-agent() {
    export AWS_PROFILE=prod-agent
    export AWS_REGION=us-east-1
    aws sso login
}

aws-release-engineer() {
    export AWS_PROFILE=release-engineer
    export AWS_REGION=us-east-1
    aws sso login
}

aws-development() {
    export AWS_PROFILE=development
    export AWS_REGION=us-east-2
    aws sso login
}

aws-staging() {
    export AWS_PROFILE=staging-agent
    export AWS_REGION=us-east-1
    aws sso login
}

aws-brazil-release() {
    export AWS_PROFILE=brazil-release
    export AWS_REGION=us-east-1
    aws sso login
}

splint-committed-check() {
    git diff --name-only @{u}..HEAD | xargs splint
}

splint-index-and-committed-check() {
    git diff --name-only --cached | xargs splint
}

splint-committed-fix() {
    git diff --name-only @{u}..HEAD | xargs splint --autocorrect
}

splint-index-and-committed-fix() {
    git diff --name-only --cached | xargs splint --autocorrect
}
